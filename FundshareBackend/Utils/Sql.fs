// original source: https://github.com/Zaid-Ajaj/Npgsql.FSharp/blob/master/src/Sql.fs
// mod: added transactions
module Fundshare.Utils.Sql

open System
open Npgsql
open System.Collections.Generic

type SqlValue =
    | Short of int16
    | Int of int
    | Long of int64
    | String of string
    | Date of DateTime
    | Bool of bool
    | Number of double
    | Decimal of decimal
    | Bytea of byte[]
    | HStore of Map<string, string>
    | Uuid of Guid
    | IntArray of int list
    | StringArray of string list
    | Null
    | Other of obj

type SqlRow = list<string * SqlValue>

type SqlTable = list<SqlRow>

type SqlResult =
  | NonQueryResult of int  // specifies count of affected rows
  | TableResult of SqlTable
  | ScalarResult of SqlValue


type SqlQuery =
  | NonQuery of query : string * parameters : (list<string * SqlValue>) // mutations or unspecified
  | TableQuery of query : string * parameters : (list<string * SqlValue>)
  | ScalarQuery of query : string * parameters : (list<string * SqlValue>)


[<RequireQualifiedAccess>]
module Sql =
    open System.Diagnostics

    type ConnectionStringBuilder = private {
        Host: string
        Database: string
        Username: string
        Password: string
        Port: int
        Config : string
    }

    type Session = {
        Connection : NpgsqlConnection
        Transaction : NpgsqlTransaction option
        //NeedPrepare : bool
    }

    let private defaultConString() : ConnectionStringBuilder = {
            Host = ""
            Database = ""
            Username = ""
            Password = ""
            Port = 5432
            Config = ""
    }

    let connect (constr: string) : Session =
        let connection = new NpgsqlConnection(constr)
        let session = { Connection = connection; Transaction = None }
        connection.Open()
        session        
        
    let unpackSqlQuery (query : SqlQuery) : string * list<string * SqlValue> =
        match query with
        | NonQuery (q, p) -> (q, p)
        | ScalarQuery (q, p) -> (q, p)
        | TableQuery (q, p) -> (q, p)
        
        
    
    let private populateCmd (cmd: NpgsqlCommand) (parameters: list<string * SqlValue>) =
        for param in parameters do
          let paramValue : obj =
            match snd param with
            | String text -> upcast text
            | Int i -> upcast i
            | Uuid x -> upcast x
            | Short x -> upcast x
            | Date date -> upcast date
            | Number n -> upcast n
            | Bool b -> upcast b
            | Decimal x -> upcast x
            | Long x -> upcast x
            | Bytea x -> upcast x
            | HStore dictionary ->
                let value =
                  dictionary
                  |> Map.toList
                  |> dict
                  |> Dictionary
                upcast value
            | IntArray arr -> upcast arr
            | StringArray arr -> upcast arr
            | Null -> null
            | Other x -> x

          let paramName = sprintf "@%s" (fst param)
          cmd.Parameters.AddWithValue(paramName, paramValue) |> ignore
          
    let readValue value =
        match box value with
          | :? int32 as x -> Int x
          | :? string as x -> String x
          | :? System.DateTime as x -> Date x
          | :? bool as x -> Bool x
          | :? int64 as x -> Long x
          | :? decimal as x -> Decimal x
          | :? double as x ->  Number x
          | :? System.Guid as x -> Uuid x
          | :? array<byte> as xs -> Bytea xs
          | :? IDictionary<string, string> as dict ->
              dict
              |> Seq.map (|KeyValue|)
              |> Map.ofSeq
              |> HStore
          | :? array<int> as xs -> IntArray (Array.toList xs)
          | :? array<string> as xs -> StringArray (Array.toList xs)
          | null -> Null
          | _ -> Other value
          
    
    let readRow (reader : NpgsqlDataReader) : SqlRow =
        let readFieldSync fieldIndex =
            let fieldName = reader.GetName(fieldIndex)
            if reader.IsDBNull(fieldIndex)
            then fieldName, Null
            else fieldName, readValue (reader.GetFieldValue(fieldIndex))

        [0 .. reader.FieldCount - 1]
        |> List.map readFieldSync

    let readTable (reader: NpgsqlDataReader) : SqlTable =
      let r = reader
      [ while reader.Read() do yield readRow reader ]

        
    let private _executeQuery (session : Session) (sqlQuery : SqlQuery) : SqlResult =
      let (query, parameters) = sqlQuery |> unpackSqlQuery
        
      use command = new NpgsqlCommand(query, session.Connection)
      do populateCmd command parameters

      if Option.isSome session.Transaction then
          do command.Transaction <- session.Transaction.Value

          //if session.NeedPrepare then do command.Prepare()
        
      try
        let result = match sqlQuery with
        | NonQuery _ -> NonQueryResult <| command.ExecuteNonQuery()
        | ScalarQuery _ -> ScalarResult <| (command.ExecuteScalar() |> readValue)
        | TableQuery _ -> TableResult <| (command.ExecuteReader() |> readTable)
        do Debug.Print (result.ToString())
        result
      with | ex ->
        Debug.Print ex.Message
        failwith ex.Message
        
    let executeQuery (query : SqlQuery) (session : Session) : Result<SqlResult, string> =
      try
        let result = query |> _executeQuery session |> Ok
        do session.Connection.Close()
        result
      with | ex ->
        do Debug.Print ex.Message
        Error ex.Message 
        
    let executeQueryAndGetRow query session =
      session |> executeQuery query
      |> function
        | Ok (TableResult (row :: [])) -> Some row
        | Ok (TableResult rows) -> failwith <| "expected to have exactly one row but got " + rows.Length.ToString()
        | _ -> None

    let executeQueryAndGetRows query session =
      session |> executeQuery query
      |> function
        | Ok (TableResult rows) -> Some rows
        | _ -> None
        
    // transactional
    let executeQueries (queries : SqlQuery list) (session : Session) : Result<SqlResult list, string> =
        use trans = session.Connection.BeginTransaction()

        try
            let results = queries |> List.map (_executeQuery session)
            trans.Commit()
            let ret =
              if trans.IsCompleted then Ok results
              else Error "transaction not completed"
            do session.Connection.Close()
            ret
        with | ex ->
            trans.Rollback()
            Error ex.Message
        
    let host x = { defaultConString() with Host = x }
    let username x con = { con with Username = x }
    let password x con = { con with Password = x }
    let database x con = { con with Database = x }
    let port n con = { con with Port = n }
    let config x con = { con with Config = x }
    let str (con:ConnectionStringBuilder) =
        sprintf "Host=%s;Username=%s;Password=%s;Database=%s;Port=%d;%s"
            con.Host
            con.Username
            con.Password
            con.Database
            con.Port
            con.Config

    let toBool = function
        | Bool x -> x
        | value -> failwithf "Could not convert %A into a boolean value" value

    let toInt = function
        | Int x -> x
        | value -> failwithf "Could not convert %A into an integer" value

    let toString = function
        | String x -> x
        | value -> failwithf "Could not convert %A into a string" value

    let toDateTime = function
        | Date x -> x
        | value -> failwithf "Could not convert %A into a DateTime" value

    let toFloat = function
        | Number x -> x
        | value -> failwithf "Could not convert %A into a floating number" value

    let multiline xs = String.concat Environment.NewLine xs

    let mapEachRow (f: SqlRow -> Option<'a>) (table: SqlTable) =
        List.choose f table