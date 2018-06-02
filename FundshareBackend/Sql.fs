// original source: https://github.com/Zaid-Ajaj/Npgsql.FSharp/blob/master/src/Sql.fs
// mod: added transactions
module Utils.Sql

open System
open Npgsql
open System.Threading.Tasks
open System.Data
open System.Collections.Generic
open FSharp.Control.Tasks

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
    | Null
    | Other of obj

type SqlRow = list<string * SqlValue>

type SqlTable = list<SqlRow>

type SqlResult =
  | NonQuery of int
  | Table of SqlTable
  | Scalar of SqlValue


[<RequireQualifiedAccess>]
module Sql =

    type ConnectionStringBuilder = private {
        Host: string
        Database: string
        Username: string
        Password: string
        Port: int
        Config : string
    }


    type SqlProps = private {
        //ConnectionString : string
        Connection : NpgsqlConnection option
        SqlQuery : string list
        Parameters : SqlRow
        IsTransaction : bool
        NeedPrepare : bool
    }
    
    type Session = {
        Connection : NpgsqlConnection
    }

    let private defaultConString() : ConnectionStringBuilder = {
            Host = ""
            Database = ""
            Username = ""
            Password = ""
            Port = 5432
            Config = ""
    }
    let private defaultProps() = {
        Connection = None
        SqlQuery = []
        Parameters = []
        IsTransaction = false
        NeedPrepare = false
    }
    
    
    let connect (constr: string) : Session =
        let props = { defaultProps() with Connection = Some <| new NpgsqlConnection(constr) }
        let connection = Option.toObj props.Connection
        connection.Open()
        { Connection = connection }
        
    let openTransaction (session : Session, fetchQueries : (string -> SqlProps)) : (SqlResult list) option =
      use trans = session.Connection.BeginTransaction()
      trans.Commit()
      trans.Connection.CreateCommand()
      

        
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

    let query (sql: string) props = { props with SqlQuery = [sql] }
    let prepare  props = { props with NeedPrepare = true}
    let queryMany queries props = { props with SqlQuery = queries }
    let parameters ls props = { props with Parameters = ls }

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

    let readValue value =
        match box value with
        | :? int32 as x -> Int x
        | :? string as x -> String x
        | :? System.DateTime as x -> Date x
        | :? bool as x ->  Bool x
        | :? int64 as x ->  Long x
        | :? decimal as x -> Decimal x
        | :? double as x ->  Number x
        | :? System.Guid as x -> Uuid x
        | :? array<byte> as xs -> Bytea xs
        | :? IDictionary<string, string> as dict ->
            dict
            |> Seq.map (|KeyValue|)
            |> Map.ofSeq
            |> HStore
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
        [ while reader.Read() do yield readRow reader ]


    let private populateCmd (cmd: NpgsqlCommand) (props: SqlProps) =
        for param in props.Parameters do
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
            | Null -> null
            | Other x -> x


          let paramName = sprintf "@%s" (fst param)
          cmd.Parameters.AddWithValue(paramName, paramValue) |> ignore

    let executeTable (props: SqlProps) : SqlTable =
        if List.isEmpty props.SqlQuery then failwith "No query provided to execute..."
        use command = new NpgsqlCommand(List.head props.SqlQuery, connection)
        if props.NeedPrepare then command.Prepare()
        populateCmd command props
        use reader = command.ExecuteReader()
        readTable reader

    let executeTableSafe (props: SqlProps) : Result<SqlTable, exn> =
        try Ok (executeTable props)
        with | ex -> Error ex


    let multiline xs = String.concat Environment.NewLine xs

//    let executeMany (props: SqlProps)  =
//        if List.isEmpty props.SqlQuery then failwith "No query provided to execute..."
//        let queryCount = List.length props.SqlQuery
//        let singleQuery = String.concat ";" props.SqlQuery
//        use connection = new NpgsqlConnection(props.ConnectionString)
//        connection.Open()
//        use command = new NpgsqlCommand(singleQuery, connection)
//        if props.NeedPrepare then command.Prepare()
//        populateCmd command props
//        use reader = command.ExecuteReader()
//        [ for _ in 1 .. queryCount do
//            yield readTable reader
//            reader.NextResult() |> ignore ]


    let executeScalar (props: SqlProps) : Sql =
        if List.isEmpty props.SqlQuery then failwith "No query provided to execute..."
        if Option.isNone props.Connection then failwith "No connection specified"
        use connection = Option.toObj props.Connection
        connection.Open()
        use command = new NpgsqlCommand(List.head props.SqlQuery, connection)
        if props.NeedPrepare then command.Prepare()
        populateCmd command props
        command.ExecuteScalar()
        |> readValue

    let executeNonQuery (props: SqlProps) : int =
        if List.isEmpty props.SqlQuery then failwith "No query provided to execute..."
        use connection = new NpgsqlConnection(props.ConnectionString)
        connection.Open()
        use command = new NpgsqlCommand(List.head props.SqlQuery, connection)
        if props.NeedPrepare then command.Prepare()
        populateCmd command props
        command.ExecuteNonQuery()

    let executeNonQuerySafe (props: SqlProps) : Result<int, exn> =
        try Ok (executeNonQuery props)
        with | ex -> Error ex

    let executeScalarSafe (props: SqlProps) : Result<Sql, exn> =
        try  Ok (executeScalar props)
        with | ex -> Error ex

    let executeScalarTask (props: SqlProps) =
        task {
            if List.isEmpty props.SqlQuery then failwith "No query provided to execute..."
            use connection = new NpgsqlConnection(props.ConnectionString)
            do! connection.OpenAsync()
            use command = new NpgsqlCommand(List.head props.SqlQuery, connection)
            if props.NeedPrepare then command.Prepare()
            do populateCmd command props
            let! value = command.ExecuteScalarAsync()
            return readValue value
        }

    let mapEachRow (f: SqlRow -> Option<'a>) (table: SqlTable) =
        List.choose f table