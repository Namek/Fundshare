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
    |> Sql.executeQuery (fun () ->
      TableQuery ("SELECT id, email, name FROM public.users", []))
    
    |> Sql.mapEachRow (function
        | [ "id", Int id
            "email", String email
            "name", String name ] ->
          Some
            { id = id
              email = email
              name = name }
        | _ -> None)
        
  
      
let addTransaction (args : Input_AddTransaction) : UserTransaction option =
  defaultConnection
    |> Sql.connect 
    |> Sql.executeQueries (fun () ->
      [ NonQuery ("INSERT INTO \"public.users\" VALUES (transaction_type, payor_id, payee_ids, tags, description, paid_at, inserted_at, updated_at)",
         [ "transactionType", Int 3
           "payorId", Int args.payorId
           "payeesIds", IntArray args.payeeIds
           "tags", StringArray args.tags
           "description", Option.map String args.description |> Option.defaultWith (fun () -> Null)
         ])
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
       TableQuery ("SELECT email, name FROM public.users WHERE id=@userId LIMIT 1", [
         "userId", Int id
       ]))
    |> function
      | Ok (TableResult (row :: [])) -> Some row
      | _ -> None
    |> function
      | Some [ "email", String email
               "name", String name ] ->
        Some { email = email; name = name; id = id } 
      | _ -> None