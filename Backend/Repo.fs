module Backend.Repo

open Backend
open System
open Backend.DataStructures
open Utils.Sql
open SqlFrags.SqlGen.Frags
open SqlFrags.SqlGen


  
let defaultConnection : string =
  Sql.host AppConfig.DB.host
  |> Sql.port AppConfig.DB.port
  |> Sql.username AppConfig.DB.username
  |> Sql.password AppConfig.DB.password
  |> Sql.database AppConfig.DB.database
//    |> Sql.config "SslMode=Require;"
  |> Sql.str

let connect = fun () -> defaultConnection |> Sql.connect

let executeNonQuery (query : string) : Result<int, string> =
  connect()
  |> Sql.executeQuery (NonQuery (query, []))
  |> function
    | Ok (NonQueryResult affectedRows) -> Ok affectedRows
    | Error err -> Error <| "query error: " + err
    | _ -> Error <| "general error for query: " + query
    
let emitSql (parts : Frag list) =
  parts |> Frags.Emit SqlSyntax.Any
    
let transformOrEmpty (transform : string -> string) (arg : string option) : string =
  (Option.defaultValue "" (arg |> Option.bind (fun q -> Some <| transform q)))

    
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
    authoredByUser1Count : int
    authoredByUser2Count : int
    inboxForUser1Count : int
    inboxForUser2Count : int
    lastUpdateAt : DateTime option }

type BalanceCorrection =
 private 
  { user1Id: int
    user2Id : int
    num : int
    den : int }
  
type Transaction =
 private
  { authorId : int
    payorId : int 
    beneficientIds : int list
    acceptanceIds : int list
    amount : int
    updatedAt : DateTime }

    

// paying for self only - no money flow
let isPayingForSelfOnly transaction =
  List.length transaction.beneficientIds = 1 && (List.first transaction.beneficientIds) = Some transaction.payorId
  
let isSharedPayment transaction =
  List.length transaction.beneficientIds > 1

let isMoneyTransfer transaction =
  List.length transaction.beneficientIds = 1 && not <| isPayingForSelfOnly transaction
  
let isUnseenForUser userId (transaction : Transaction) : bool =
  not <| List.contains userId transaction.acceptanceIds


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

let update (table : string) (where : string option) (values : (string * string) list) =
  [
    (Table table).Update values
    WhereS (where |> Option.defaultValue "")
  ]
  |> emitSql
  |> executeNonQuery
  
// balance: how much the left is rebalanced to the right
// e.g. if num/den > 0 then user2 (right) is in debt to user1 (left)
let calcTransactionBalanceCorrections transaction : BalanceCorrection list =
  let peopleCount =
    transaction.payorId :: transaction.beneficientIds
    |> List.distinct
    |> List.length
  
  transaction.beneficientIds
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
      "SELECT author_id, payor_id, beneficient_ids, acceptance_ids, amount, updated_at
       FROM public.transactions
       WHERE payor_id = @u1 OR payor_id = @u2 OR @u1 = any(beneficient_ids) OR @u2 = any(beneficient_ids)",
      [ "u1", QInt user1Id; "u2", QInt user2Id ] )) (connect())
    |> function
      | Ok (TableResult rows) -> rows
      | _ -> failwith "---"
    |> Sql.mapEachRow (function
      | [ "author_id", QInt authorId
          "payor_id", QInt payorId
          "beneficient_ids", QIntArray beneficientIds
          "acceptance_ids", QIntArray acceptanceIds
          "amount", QInt amount
          "updated_at", QDate updatedAt
        ] -> Some <|
          { authorId = authorId
            payorId = payorId
            beneficientIds = beneficientIds
            acceptanceIds = acceptanceIds
            amount = amount
            updatedAt = updatedAt }
      | _ -> failwith "couldnt get transactions for 2 users")
      
  let mutable stats = (0, 0, 0, 0, 0, 0, (if transactions.IsEmpty then None else Some transactions.Head.updatedAt))
  
  stats <- transactions
    |> List.fold (fun acc_stats trans ->
        let (
          _isSharedPayment,
          _isMoneyTransfer,
          _authoredByUser1,
          _authoredByUser2,
          _isUnseenForUser1,
          _isUnseenForUser2,
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
           _authoredByUser1 + (if trans.authorId = user1Id then 1 else 0),
           _authoredByUser2 + (if trans.authorId = user2Id then 1 else 0),
           _isUnseenForUser1 + (if isUnseenForUser user1Id trans then 1 else 0),
           _isUnseenForUser2 + (if isUnseenForUser user2Id trans then 1 else 0),
           lastUpdatedAt)
  
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
      {num = 0; den = 1})
      
  let (s1, s2, s3, s4, s5, s6, d) = stats

  { num = totalBalance.num
    den = totalBalance.den
    user1Id = expectedUser1Id
    user2Id = expectedUser2Id
    sharedPaymentCount = s1
    moneyTransferCount = s2
    authoredByUser1Count = s3
    authoredByUser2Count = s4
    inboxForUser1Count = s5
    inboxForUser2Count = s6
    lastUpdateAt = d   
  }
  

let calculateBalanceForUsers (userIds : int list) =
  let ids = List.distinct userIds
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
        INSERT INTO public.balances
         (user1_id, user2_id, balance_num, balance_den, user1_has_more, shared_payment_count,
          transfer_count, authored_by_user1_count, authored_by_user2_count,
          inbox_for_user1_count, inbox_for_user2_count, last_update_at, inserted_at, updated_at)
        
        VALUES
         (@user1Id, @user2Id, @num, @den, @user1HasMore, @spc, @tc, @au1c, @au2c, @uu1c, @uu2c, @lastUpdate, @timeNow, @timeNow)
        
        ON CONFLICT (user1_id, user2_id) DO UPDATE SET
          balance_num=@num, balance_den=@den, user1_has_more=@user1HasMore, shared_payment_count = @spc,
          transfer_count=@tc, authored_by_user1_count=@au1c, authored_by_user2_count=@au2c,
          inbox_for_user1_count=@uu1c, inbox_for_user2_count=@uu2c, last_update_at=@lastUpdate, updated_at=@timeNow;
        ",
        [ "user1Id", QInt b.user1Id
          "user2Id", QInt b.user2Id
          "num", QInt (abs b.num)
          "den", QInt b.den
          "user1HasMore", QBool (b.num > 0)
          "spc", QInt b.sharedPaymentCount
          "tc", QInt b.moneyTransferCount
          "au1c", QInt b.authoredByUser1Count
          "au2c", QInt b.authoredByUser2Count
          "uu1c", QInt b.inboxForUser1Count
          "uu2c", QInt b.inboxForUser2Count
          "lastUpdate", if b.lastUpdateAt.IsSome then QDate b.lastUpdateAt.Value else QNull
          "timeNow", QDate DateTime.Now
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
      | [ "id", QInt id
          "email", QString email
          "name", QString name
        ] ->
          { id = id
            email = email
            name = name
            balances = None
            transactions = None } |> Some
      | _ -> None)
      
let addTransaction (args : Input_AddTransaction) : Result<UserTransaction, String> =
  let now = DateTime.Now
  let acceptanceIds = [args.authorId]

  ( if List.isEmpty args.beneficientIds then
      Error "At least one beneficient has to be specified."
    else
      Ok true )
  |> Result.bind (fun _ ->
    connect()
    |> Sql.executeQueries
      [ ScalarQuery (
          "INSERT INTO public.transactions
             (amount, author_id, payor_id, beneficient_ids, acceptance_ids, tags, description, inserted_at, updated_at)
           VALUES (@amount, @aid, @pid, @bids, @aids, @tags, @descr, @timeNow, @timeNow) RETURNING id",
          [ "amount", QInt args.amount
            "aid", QInt args.authorId
            "pid", QInt args.payorId
            "bids", QIntArray args.beneficientIds
            "aids", QIntArray acceptanceIds
            "tags", QStringArray args.tags
            "descr", args.description |> Option.map QString |> Option.defaultWith (fun () -> QNull)
            "timeNow", QDate <| now ] ) ]
  )
  |> function
    | Ok [ScalarResult (QInt transactionId) ] ->
        { id = transactionId
          authorId = args.authorId
          amount = args.amount
          payorId = args.payorId
          beneficientIds = args.beneficientIds
          acceptanceIds = acceptanceIds
          tags = args.tags
          description = args.description
          insertedAt = now
          beneficients = None } |> Ok
        
    | Error err -> Error (err.ToString())
  |> (fun ret ->
    // TODO in some future you may want to improve performance of this update query.
    // It's actually quite costly to calculate balance based on all transactions when we add just one transaction.
    do updateBalanceForUsers (args.payorId :: args.authorId :: args.beneficientIds) |> ignore

    ret
  )


let getUserById (id : int) : User option =
  connect()
  |> Sql.executeQueryAndGetRow
    (TableQuery ("SELECT email, name FROM public.users WHERE id=@userId LIMIT 1",
                 ["userId", QInt id]))
  |> function
    | Some [ "email", QString email; "name", QString name ] ->
      Some { email = email; name = name; id = id; balances = None; transactions = None } 
    | _ -> None
    
let validateUserCredentials (email : string) (passwordSalted : String) : User option =
  connect()
  |> Sql.executeQueryAndGetRow
    (TableQuery ("SELECT id, name FROM public.users WHERE email=@email AND password_hash=@password LIMIT 1;",
                 ["email", QString email; "password", QString (String.toLowerInvariant passwordSalted)]))
  |> function
    | (Some [ "id", QInt id; "name", QString name ]) ->
      Some { id = id; name = name; email = email; balances = None; transactions = None }
    | _ -> None

let addUser (email : string) (passwordHash : string) (name : String) : int option =
  let now = DateTime.Now
  connect()
  |> Sql.executeQueryAndGetRow (
    ScalarQuery (
      "INSERT INTO public.users (email, name, password_hash, inserted_at, updated_at)
       VALUES (@email, @name, @passwordHash, @now) RETURNING id",
      ["email", QString email; "name", QString name; "passwordHash", QString passwordHash; "now", QDate now]
    ))
  |> function 
    | Some [ "id", QInt id ] ->
        Some id
    | _ -> None
    
    
let getTransactions (where : string option) (orderBy : string option) (offset : int option) (limit : int option) : UserTransaction list =
  let baseQuery =
    "SELECT id, author_id, payor_id, beneficient_ids, acceptance_ids, amount, description, tags, inserted_at
     FROM public.transactions"
     
  let finalQuery = baseQuery +
    (where |> transformOrEmpty (fun q -> " WHERE " + q)) +
    (orderBy |> transformOrEmpty (fun o -> " ORDER BY " + o)) +
    (offset |> Option.map Convert.ToString |> transformOrEmpty (fun off -> " OFFSET " + off)) +
    (limit |>  Option.map Convert.ToString |> transformOrEmpty (fun lim -> " LIMIT " + lim))
  
  connect()
    |> Sql.executeQueryAndGetRows (TableQuery (finalQuery, []))
    |> (Option.map << Sql.mapEachRow) (
      function
      | [ "id", QInt tid
          "author_id", QInt authorId
          "payor_id", QInt pid
          "beneficient_ids", QIntArray pids
          "acceptance_ids", QIntArray aids
          "amount", QInt amount
          "description", maybeDescr
          "tags", QStringArray tags
          "inserted_at", QDate insertedAt
        ] ->        
          let descr : string option = match maybeDescr with
          | QString descrStr -> Some descrStr
          | _ -> None
    
          Some <|
          { id = tid
            authorId = authorId
            payorId = pid
            beneficientIds = pids
            acceptanceIds = aids
            amount = amount
            description = descr
            tags = tags
            insertedAt = insertedAt
            beneficients = None}
            
      | _ -> failwith "user transactions not read from DB properly"
    )
    |> Option.defaultValue []
    
    
let getUserTransactions (userId : int) (where : string option) (offset : int option) (limit : int option) : UserTransaction list =
  let uid = userId.ToString()
  let qwhere =
    "(payor_id = " + uid + " OR " + uid + " = any(beneficient_ids))"
    + (where |> transformOrEmpty (fun w -> " AND " + w))

  getTransactions (Some qwhere) (Some "inserted_at DESC, id DESC") offset limit
 
// returns IDs of updated transactions
let acceptTransactions (userId : int) (transactionIds : int list) : int list =
  let uid = userId.ToString()
  let tids = System.String.Join(",", transactionIds)
  let query =
    "UPDATE transactions SET" +
    " acceptance_ids = acceptance_ids || " + uid + ", updated_at = NOW()" +
    "WHERE" +
    " id = any('{" + tids + "}'::int[])" +
    " AND (" + uid + " = any(beneficient_ids) OR " + uid + " = payor_id)" + 
    " AND " + uid + " != all(acceptance_ids)" +
    "RETURNING id"

  connect()
  |> Sql.executeQueryAndGetRows (TableQuery (query, []))
  |> Option.map (Sql.mapEachRow (function | ["id", QInt id] -> Some id))
  |> Option.map (fun acceptedIds ->
    let inboxDiff = (List.length acceptedIds).ToString()
    let query =
      "UPDATE balances SET" +
      " inbox_for_user1_count = " +
      "  (CASE WHEN user1_id = " + uid + " THEN GREATEST(0, inbox_for_user1_count - " + inboxDiff + ") ELSE inbox_for_user1_count END)" +
      ",inbox_for_user2_count = " +
      "  (CASE WHEN user2_id = " + uid + " THEN GREATEST(0, inbox_for_user2_count - " + inboxDiff + ") ELSE inbox_for_user2_count END)" +
      "WHERE (user1_id = " + uid + " OR user2_id = " + uid + ")"

    do connect()
    |> Sql.executeQuery (NonQuery (query, []))
    |> ignore

    acceptedIds
  )
  |> Option.defaultValue []
  
let getUserBalances userId : BalanceToOtherUser list =
  connect()
  |> Sql.executeQueryAndGetRows (TableQuery (
    "(SELECT user2_id as other_user_id, balance_num, balance_den, user1_has_more as sign, shared_payment_count,
     	transfer_count, last_update_at,
     	authored_by_user1_count as authored_by_me_count,
     	(shared_payment_count + transfer_count - authored_by_user1_count) as authored_by_other_user_count,
     	inbox_for_user1_count as inbox_for_me_count, inbox_for_user2_count as inbox_for_other_user_count
     FROM public.balances
     WHERE user1_id = @uid)
     UNION
     (SELECT user1_id as other_user_id, balance_num, balance_den, (not user1_has_more) as sign, shared_payment_count,
     	transfer_count, last_update_at,
     	(shared_payment_count + transfer_count - authored_by_user1_count) as authored_by_me_count,
     	authored_by_user1_count as authored_by_other_user_count,
     	inbox_for_user2_count as inbox_for_me_count, inbox_for_user1_count as inbox_for_other_user_count
     FROM public.balances
     WHERE user2_id = @uid)", ["uid", QInt userId]))
  |> (Option.map << Sql.mapEachRow) (
    function
    | [ "other_user_id", QInt otherUserId
        "balance_num", QInt num
        "balance_den", QInt den
        "sign", QBool signBool
        "shared_payment_count", QInt sharedPaymentCount
        "transfer_count", QInt transferCount
        "last_update_at", QDate lastUpdateAt
        "authored_by_me_count", QInt authoredByMeCount
        "authored_by_other_user_count", QInt authoredByOtherUserCount
        "inbox_for_me_count", QInt inboxForMeCount
        "inbox_for_other_user_count", QInt inboxForOtherUsercount
      ] ->
        let sign = if signBool then 1 else -1
        Some <|
          { otherUserId = otherUserId
            value = (float num) / (float (100*den))
            iHaveMore = signBool
            sharedPaymentCount = sharedPaymentCount
            transferCount = transferCount
            authoredByMeCount = authoredByMeCount
            authoredByOtherUserCount = authoredByOtherUserCount
            inboxForMeCount = inboxForMeCount
            inboxForOtherUserCount = inboxForOtherUsercount
            lastUpdateAt = lastUpdateAt
            otherUser = None
          }
    | _ -> failwith "no match for this list"
  )
  |> Option.defaultValue []