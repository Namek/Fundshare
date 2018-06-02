module Fundshare.Repo

open Fundshare.DataStructures
open Utils.Sql
open NpgsqlTypes

  
let defaultConnection : string =
  Sql.host "localhost"
    |> Sql.port 5432
    |> Sql.username "postgres"
    |> Sql.password "postgres"
    |> Sql.database "fundshare_dev"
//      |> Sql.config "SslMode=Require;" // optional Config for connection string
    |> Sql.str
    

let getAllUsers() : User list =
  defaultConnection
    |> Sql.connect
    
    |> Sql.query "SELECT id, email, name FROM public.users"
    |> Sql.executeTable // SqlTable
    |> Sql.mapEachRow (function
        | [ "id", Int id
            "email", String email
            "name", String name ] ->
          Some
            { id = id
              email = email
              name = name }
        | _ -> None)
        

//let getUserById (id : int) : User option =
//  defaultConnection
//    |> Sql.connect
//    |> Sql.query ("SELECT email, name FROM public.users WHERE id=" + id.ToString())
//    |> Sql.executeTable
//    |> Sql.mapEachRow 
//      (function
//        | [ "email", String email
//            "name", String name ] ->
//          Some
//            { id = id
//              email = email
//              name = name }
//        | _ -> None)
//    |> List.first


//let addTransaction (args) : UserTransaction option =
//  defaultConnection
//      |> Sql.connect
//      |> Sql.query "INSERT INTO \"public.users\"
//        (transaction_type, payor_id, payee_ids, tags, description, paid_at, inserted_at, updated_at)
//        VALUES (@transactionType, @payorId, @payeeIds, @tags, @description, @paidAt, @time, @time)"
//      |> Sql.parameters
////        [ "transactionType, 
//      |> Sql.executeNonQuery
      
      
  
      
let addTransaction (args) : UserTransaction option =
  defaultConnection
    |> Sql.connect 
    |> Sql.executeQueries (fun () ->
      [ NonQuery ("INSERT INTO \"public.users\"", [])
        NonQuery ("UPDATE balance", [])
      ])
    |> function
      | Ok [NonQueryResult q1; NonQueryResult q2] ->
          if q1+q2 > 0 then Some args else None
      | _ -> None


let getUserById (id : int) : User option =
  defaultConnection
    |> Sql.connect
    |> Sql.executeQuery (fun () ->
       TableQuery ("SELECT", []))
    |> function
      | Ok (TableResult rows) -> Some rows
      | _ -> None
    |> Option.map (fun rows ->
      rows.Head
    )