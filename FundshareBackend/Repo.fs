module Fundshare.Repo

open Fundshare
open System
open Fundshare.DataStructures
open Utils.Sql
open NpgsqlTypes
open Utils.Sql
open Utils.Sql
open Utils.Sql

  
let defaultConnection : string =
  Sql.host "localhost"
    |> Sql.port 5432
    |> Sql.username "postgres"
    |> Sql.password "postgres"
    |> Sql.database "fundshare_dev"
//      |> Sql.config "SslMode=Require;"
    |> Sql.str
    
    
type Fraction =
 private 
  { num : int
    den : int }

type Balance =
 private 
  { user1Id: int
    user2Id : int
    num : int
    den : int }
  
type Transaction =
 private
  { transactionType : TransactionType
    payorId : int 
    payeeIds : int list
    amount : int
    updatedAt : DateTime }
    
type Stats =
 private
  { isPayingForSelfOnly : int
    isSharedPayment : int
    isMoneyTransfer : int }
    

// paying for self only - no money flow
let isPayingForSelfOnly transaction =
  List.length transaction.payeeIds = 1 && (List.first transaction.payeeIds) = Some transaction.payorId
  
let isSharedPayment transaction =
  List.length transaction.payeeIds > 1

let isMoneyTransfer transaction =
  List.length transaction.payeeIds = 1 && not <| isPayingForSelfOnly transaction

// greatest common denominator
let rec gcd (a : int) (b : int) : int =
  match a, b with
  | a, 0 -> a
  | 0, b -> b
  | a, b -> gcd b (a % b)
  
// least common multiple
let lcm (a : int) (b : int) : int =
  match a, b with
  | 0, 0 -> 0
  | a, b -> a*b / gcd a b
  
// balance: how much the left is rebalanced to the right
// e.g. if num/den > 0 then user2 (right) is in debt to user1 (left)
let calcTransactionBalances transaction : Balance list =
  let peopleCount =
    transaction.payorId :: transaction.payeeIds
    |> List.distinct
    |> List.length
  
  transaction.payeeIds
    |> List.filter (fun id -> id <> transaction.payorId)
    |> List.map (fun id ->
      let b =
        { user1Id = transaction.payorId
          user2Id = id
          num = transaction.amount
          den = peopleCount }
          
      // always contain {smaller_id, bigger_id} tuple for user ids
      if b.user1Id < b.user2Id then b
      else { user1Id = b.user2Id; user2Id = b.user1Id; num = -b.num; den = b.den }
    )
    
    

let calculateBalanceFor2Users conn user1Id user2Id =
  let transactions : Transaction list  =
    Sql.executeQuery (TableQuery (
      "SELECT transaction_type, payor_id, payee_ids, amount, updatedAt FROM public.transactions
       WHERE payor_id = @u1 OR payor_id = @u2 OR @u1 IN payee_ids OR @u2 IN payee_ids",
      ["u1", Int user1Id; "u2", Int user2Id] )) (Sql.connect conn)
    |> function
      | Ok (TableResult rows) -> rows
      | _ -> failwith "---"
    |> Sql.mapEachRow (function
      | [ "transaction_type", String tt
          "payor_id", Int payorId
          "payee_ids", IntArray payeeIds
          "amount", Int amount
          "updatedAt", Date updatedAt
        ] -> Some <|
          { transactionType = decodeTransactionType tt
            payorId = payorId
            payeeIds = payeeIds
            amount = amount
            updatedAt = updatedAt }
      | _ -> failwith "couldnt get transactions for 2 users")
      
  let mutable stats = (0, 0, 0, (if transactions.IsEmpty then None else Some transactions.Head.updatedAt))
  
  stats <- transactions
    |> List.fold (fun acc_stats trans ->
      let (
        _isPayingForSelfOnly,
        _isSharedPayment,
        _isMoneyTransfer,
        _lastUpdateAt
        ) = acc_stats
  
      let lastUpdatedAt = (
        if _lastUpdateAt.IsSome then
          if (trans.updatedAt > _lastUpdateAt.Value) then (Some trans.updatedAt) else _lastUpdateAt
        else
          _lastUpdateAt)
    
      let newStats = 
        (_isPayingForSelfOnly + (if isPayingForSelfOnly trans then 1 else 0),
         _isSharedPayment + (if isSharedPayment trans then 1 else 0),
         _isMoneyTransfer + (if isMoneyTransfer trans then 1 else 0),
         _lastUpdateAt)

      newStats
      ) stats
    
  let (expectedUser1Id, expectedUser2Id) = if user1Id < user2Id then (user1Id, user2Id) else (user2Id, user1Id)
  
  // transactions may contain other users than user1 and user2, so filter them out
  let balanceCorrections : Balance list =
    transactions
    |> List.fold (fun allCollected trans ->
         List.concat [(calcTransactionBalances trans); allCollected]) []
    |> List.filter (fun balance ->
         balance.user1Id = expectedUser1Id && balance.user2Id = expectedUser2Id)
    
  let totalBalance =
    balanceCorrections
    |> List.fold
      (fun balances state -> )
      
    
  []  
  

let calculateBalanceForUsers conn userIds =
  []
    
let calculateBalanceForAllUsers conn =
  conn
  |> Sql.connect
  |> Sql.executeQuery (TableQuery ("SELECT id FROM public.users", []))
  |> function
    | Ok (TableResult rows) ->
      Some <| Sql.mapEachRow (fun row ->
        let (key, value) = row.Head
        Some <| Sql.toInt value 
      )
    | _ -> None


let updateBalances conn balances =
  conn

let updateBalanceForAllUsers conn =
  calculateBalanceForAllUsers conn
  |> updateBalances conn
  
let updateBalanceForUsers conn userIds =
  []


let getAllUsers() : User list =
  defaultConnection
  |> Sql.connect
  |> Sql.executeQuery (TableQuery ("SELECT id, email, name FROM public.users", []))
  |> function
    | Ok (TableResult (rows)) -> rows
    | _ -> []
  |> Sql.mapEachRow (function
      | [ "id", Int id
          "email", String email
          "name", String name
        ] ->
          { id = id
            email = email
            name = name } |> Some
      | _ -> None)
      
let addTransaction (args : Input_AddTransaction) : UserTransaction option =
  let now = DateTime.Now
  defaultConnection
  |> Sql.connect 
  |> Sql.executeQueries
    [ ScalarQuery (
        "INSERT INTO \"public.users\" (transaction_type, payor_id, payee_ids, tags, description, inserted_at, updated_at)
         VALUES (@tt, @pid, @pids, @tags, @descr, @timeNow, @timeNow) RETURNING id",
        [ "tt", Int 3 // TODO enum!
          "pid", Int args.payorId
          "pidds", IntArray args.payeeIds
          "tags", StringArray args.tags
          "descr", Option.map String args.description |> Option.defaultWith (fun () -> Null)
          "timeNow", Date <| now ])

      NonQuery ("UPDATE balance", [])
    ]
  |> function
    | Ok [ScalarResult (Int transactionId); NonQueryResult q2] ->
        { id = transactionId
          amount = args.amount
          payorId = args.payorId
          payeeIds = args.payeeIds
          tags = args.tags
          description = args.description
          insertedAt = now } |> Some   
        
    | _ -> None


let getUserById (id : int) : User option =
  defaultConnection
  |> Sql.connect
  |> Sql.executeQuery
    (TableQuery ("SELECT email, name FROM public.users WHERE id=@userId LIMIT 1",
                 ["userId", Int id]))
  |> function
    | Ok (TableResult (row :: [])) -> Some row
    | _ -> None
  |> function
    | Some [ "email", String email
             "name", String name ] ->
      Some { email = email; name = name; id = id } 
    | _ -> None