module Fundshare.Repo

open Fundshare
open System
open Fundshare.DataStructures
open Utils.Sql

  
let defaultConnection : string =
  Sql.host AppConfig.DB.host
  |> Sql.port AppConfig.DB.port
  |> Sql.username AppConfig.DB.username
  |> Sql.password AppConfig.DB.password
  |> Sql.database AppConfig.DB.database
//    |> Sql.config "SslMode=Require;"
  |> Sql.str
    
let connect = fun () -> defaultConnection |> Sql.connect
    
    
type Fraction =
 private 
  { num : int
    den : int }
    
type BalanceDao =
 private
  { user1Id : int
    user2Id : int
    num : int
    den : int
    sharedPaymentCount : int
    moneyTransferCount : int
    lastUpdateAt : DateTime option }

type BalanceCorrection =
 private 
  { user1Id: int
    user2Id : int
    num : int
    den : int }
  
type Transaction =
 private
  { payorId : int 
    payeeIds : int list
    amount : int
    updatedAt : DateTime }

    

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
let calcTransactionBalanceCorrections transaction : BalanceCorrection list =
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
    
    

let calculateBalanceFor2Users (user1Id : int) (user2Id : int) : BalanceDao =
  let transactions : Transaction list  =
    Sql.executeQuery (TableQuery (
      "SELECT payor_id, payee_ids, amount, updated_at FROM public.transactions
       WHERE payor_id = @u1 OR payor_id = @u2 OR @u1 = any(payee_ids) OR @u2 = any(payee_ids)",
      [ "u1", Int user1Id; "u2", Int user2Id ] )) (connect())
    |> function
      | Ok (TableResult rows) -> rows
      | _ -> failwith "---"
    |> Sql.mapEachRow (function
      | [ "payor_id", Int payorId
          "payee_ids", IntArray payeeIds
          "amount", Int amount
          "updatedAt", Date updatedAt
        ] -> Some <|
          { payorId = payorId
            payeeIds = payeeIds
            amount = amount
            updatedAt = updatedAt }
      | _ -> failwith "couldnt get transactions for 2 users")
      
  let mutable stats = (0, 0, (if transactions.IsEmpty then None else Some transactions.Head.updatedAt))
  
  stats <- transactions
    |> List.fold (fun acc_stats trans ->
        let (
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
          (_isSharedPayment + (if isSharedPayment trans then 1 else 0),
           _isMoneyTransfer + (if isMoneyTransfer trans then 1 else 0),
           _lastUpdateAt)
  
        newStats
      ) stats
    
  let (expectedUser1Id, expectedUser2Id) = if user1Id < user2Id then (user1Id, user2Id) else (user2Id, user1Id)
  
  // transactions may contain other users than user1 and user2, so filter them out
  let balanceCorrections : BalanceCorrection list =
    transactions
    |> List.fold (fun allCollected trans ->
         List.concat [(calcTransactionBalanceCorrections trans); allCollected]) []
    |> List.filter (fun balance ->
         balance.user1Id = expectedUser1Id && balance.user2Id = expectedUser2Id)
    
  let totalBalance : Fraction =
    balanceCorrections
    |> (List.fold (fun state correct ->
        let n = state.num*correct.den + correct.num*state.den
        let d = state.den*correct.den
        let divisor = gcd n d
        {num = n / divisor; den = d / divisor}
      )
      {num = 0; den = 0})
      
  let (s1, s2, d) = stats

  { num = totalBalance.num
    den = totalBalance.den
    user1Id = expectedUser1Id
    user2Id = expectedUser2Id
    sharedPaymentCount = s1
    moneyTransferCount = s2
    lastUpdateAt = d   
  }
  

let calculateBalanceForUsers (userIds : int list) =
  let idPairs = [
    for i in userIds do
      for j in userIds do
        if i < j then yield (i, j) ]
  
  idPairs |> Seq.map (uncurry calculateBalanceFor2Users)
    

let calculateBalanceForAllUsers = fun () ->
  connect()
  |> Sql.executeQuery (TableQuery ("SELECT id FROM public.users", []))
  |> function
    | Ok (TableResult rows) ->
        rows |> Sql.mapEachRow (fun row ->
          let (key, value) = row.Head
          Some <| Sql.toInt value 
        )
    | _ -> failwith "omg"
  |> calculateBalanceForUsers


let updateBalances (balances : BalanceDao seq) =
  let queries =
    balances
    |> Seq.map (fun b ->
      NonQuery ("
        INSERT INTO
          balances (user1_id, user2_id, balance_num, balance_den, user1_has_more, shared_payment_count,
                    transfer_count, unseen_update_count, last_update_at, inserted_at, updated_at)
          VALUES (@user1Id, @user2Id, @num, @den, @user1HasMore, @spc, @tc, 0, @lastUpdate, @timeNow, @timeNow)
        ON DUPLICATE KEY UPDATE
          balance_num=@num, balance_den=@den, user1_has_more=@user1HasMore, shared_payment_count = @spc,
          transfer_count=@tc, last_update_at=@lastUpdate, updated_at=@timeNow;
        ",
        [ "user1Id", Int b.user1Id
          "user2Id", Int b.user2Id
          "num", Int (abs b.num)
          "den", Int b.den
          "user1HasMore", Bool (b.num > 0)
          "spc", Int b.sharedPaymentCount
          "tc", Int b.moneyTransferCount
          "lastUpdate", if b.lastUpdateAt.IsSome then Date b.lastUpdateAt.Value else Null
          "timeNow", Date DateTime.Now
        ] ))
      |> Seq.toList

  connect()
  |> Sql.executeQueries queries

let updateBalanceForAllUsers = fun () ->
  calculateBalanceForAllUsers()
  |> updateBalances
  
let updateBalanceForUsers userIds =
  calculateBalanceForUsers userIds
  |> updateBalances


let getAllUsers() : User list =
  connect()
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
            name = name
            balances = None
            transactions = None } |> Some
      | _ -> None)
      
let addTransaction (args : Input_AddTransaction) : UserTransaction option =
  let now = DateTime.Now
  connect()
  |> Sql.executeQueries
    [ ScalarQuery (
        "INSERT INTO \"public.users\" (payor_id, payee_ids, tags, description, inserted_at, updated_at)
         VALUES (@tt, @pid, @pids, @tags, @descr, @timeNow, @timeNow) RETURNING id",
        [ "pid", Int args.payorId
          "pidds", IntArray args.payeeIds
          "tags", StringArray args.tags
          "descr", Option.map String args.description |> Option.defaultWith (fun () -> Null)
          "timeNow", Date <| now ])

        // TODO !
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
  connect()
  |> Sql.executeQueryAndGetRow
    (TableQuery ("SELECT email, name FROM public.users WHERE id=@userId LIMIT 1",
                 ["userId", Int id]))
  |> function
    | Some [ "email", String email; "name", String name ] ->
      Some { email = email; name = name; id = id; balances = None; transactions = None } 
    | _ -> None
    
let validateUserCredentials (email : string) (passwordSalted : String) : User option =
  connect()
  |> Sql.executeQueryAndGetRow
    (TableQuery ("SELECT id, name FROM public.users WHERE email=@email AND password_hash=@password LIMIT 1;",
                 ["email", String email; "password", String passwordSalted]))
  |> function
    | (Some [ "id", Int id; "name", String name ]) ->
      Some { id = id; name = name; email = email; balances = None; transactions = None }
    | _ -> None

let addUser (email : string) (passwordHash : string) (name : String) : int option =
  let now = DateTime.Now
  connect()
  |> Sql.executeQueryAndGetRow (
    ScalarQuery (
      "INSERT INTO public.users (email, name, password_hash, inserted_at, updated_at)
       VALUES (@email, @name, @passwordHash, @now) RETURNING id",
      ["email", String email; "name", String name; "passwordHash", String passwordHash; "now", Date now]
    ))
  |> function 
    | Some [ "id", Int id ] ->
        Some id
    | _ -> None
    
// TODO map graphql selectionSet to list of fields
    
let getUserTransactions (userId : int) : UserTransaction list =
  connect()
  |> Sql.executeQueryAndGetRows (TableQuery (
    "SELECT id, payor_id, payee_ids, amount, description, tags, inserted_at FROM public.transactions
     WHERE payor_id = @uid OR @uid IN payee_ids",
    ["uid", Int userId] ))
  |> (Option.map << Sql.mapEachRow) (
        function
        | [
          "id", Int tid
          "payor_id", Int pid
          "payee_ids", IntArray pids
          "amount", Int amount
          "description", maybeDescr
          "tags", StringArray tags
          "inserted_at", Date insertedAt ] ->
            
          let descr : string option = match maybeDescr with
          | String descrStr -> Some descrStr
          | _ -> None

          
          Some <|
          { id = tid
            payorId = pid
            payeeIds = pids
            amount = amount
            description = descr
            tags = tags
            insertedAt = insertedAt }
    )
  |> Option.defaultValue []
  
let getUserBalances userId : BalanceToOtherUser list =
  connect()
  |> Sql.executeQueryAndGetRows (TableQuery (
    "(SELECT user2_id as other_user_id, balance_num, balance_den, (not user1_has_more) as sign, shared_payment_count,
     	transfer_count, unseen_update_count, last_update_at
     FROM public.balances
     WHERE user1_id = @uid)
     UNION
     (SELECT user1_id as other_user_id, balance_num, balance_den, user1_has_more as sign, shared_payment_count,
     	transfer_count, unseen_update_count, last_update_at
     FROM public.balances
     WHERE user2_id = @uid)", ["uid", Int userId]))
  |> (Option.map << Sql.mapEachRow) (
    function
    | [ "other_user_id", Int otherUserId
        "balance_num", Int num
        "balance_den", Int den
        "sign", Bool signBool
        "shared_payment_count", Int sharedPaymentCount
        "transfer_count", Int transferCount
        "unseen_update_count", Int unseenUpdateCount
        "last_update_at", Date lastUpdateAt
      ] ->
        let sign = if signBool then 1 else -1
        Some <|
          { otherUserId = otherUserId
            value = (float num) / (float den)
            iHaveMore = signBool
            sharedPaymentCount = sharedPaymentCount
            transferCount = transferCount
            unseenUpdateCount = unseenUpdateCount
            lastUpdateAt = lastUpdateAt
            otherUser = None
          }
    | _ -> None
  )
  |> Option.orDefault (fun () -> [])