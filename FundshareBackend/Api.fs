module Fundshare.Api

open System
open FSharp.Data.GraphQL
open FSharp.Data.GraphQL.Parser
open FSharp.Data.GraphQL.Types
open FSharp.Data.GraphQL.Execution
open FSharp.Data.GraphQL.Linq
open Fundshare

open Paseto.Authentication
open Fundshare.DataStructures
open FundshareBackend.Utils
open YoLo


type SignInResult =
  { id : int
    name : string
    email : string
    inboxSize : int }

type SignOutResult =
  { userId : int option }
  
type CheckSessionResult =
  { id : int
    name : string
    email : string
    inboxSize : int }
  
type RegisterUserResult =
  { id : int }
  


/// PASETO
let hexToBytes (hexString : string) =
  let bytes : byte array = Array.zeroCreate (hexString.Length/2)
  
  for i in [0..(bytes.Length-1)] do
    do bytes.[i] <- Convert.ToByte(hexString.Substring(i * 2, 2), 16)
    
  bytes
  
let tokenEncryptionKey = hexToBytes(AppConfig.Auth.tokenEncryptionKey);

let createToken (userId : int) =
  let now = DateTimeOffset.Now
  
  let claims = new PasetoInstance(
    Issuer = AppConfig.Auth.tokenIssuer,
    Subject = userId.ToString(),
    Audience = "audience",
    Expiration = System.Nullable(now.AddMinutes(72. * 60.)),
    NotBefore = System.Nullable(now.AddMinutes(-10.)),
    IssuedAt = System.Nullable(now)
//      AdditionalClaims = Map.ofSeq ["roles", upcast ["Admin", "User"]],
//      Footer = Map.ofSeq ["kid", upcast "dpm0"]
  )
  PasetoUtility.Encrypt(tokenEncryptionKey, claims);

let parseToken token : PasetoInstance option =
  let value = PasetoUtility.Decrypt(tokenEncryptionKey, token)
  if value <> null then Some value else None
  
  
/////////////////////////

let rec User = Define.Object<User>("User", fieldsFn = fun () -> [
  Define.AutoField("id", Int)
  Define.AutoField("email", String)
  Define.AutoField("name", String)
  Define.Field("balances", ListOf BalanceToOtherUser, fun ctx user -> Repo.getUserBalances user.id )
  Define.Field("transactions", ListOf UserTransaction, "Transactions for which this user was a payor or a beneficient",
    [ Define.Input("offset", Nullable Int, Some 0)
      Define.Input("limit", Nullable Int, Some 20)
    ],
    fun ctx user ->
      let offset = ctx.Arg "offset"
      let limit = ctx.Arg "limit"
      Repo.getUserTransactions user.id None offset limit
  )
])
and UserTransaction = Define.Object<UserTransaction>("UserTransaction", fieldsFn = fun () -> [
  Define.AutoField("id", Int)
  Define.AutoField("payorId", Int)
  Define.AutoField("beneficientIds", ListOf Int)
  Define.AutoField("acceptanceIds", ListOf Int)
  Define.AutoField("amount", Int)
  Define.AutoField("tags", ListOf String)
  Define.AutoField("description", Nullable String)
  Define.AutoField("insertedAt", Date)
  Define.Field("beneficients", ListOf User, fun ctx transaction ->
    transaction.beneficientIds |> List.map Repo.getUserById |> List.choose id )
])
and SignInResult = Define.Object<SignInResult>("SignInResult", [
  Define.AutoField("id", Int)
  Define.AutoField("name", String)
  Define.AutoField("email", String)
  Define.AutoField("inboxSize", Int)
])
and SignOutResult = Define.Object<SignOutResult>("SignOutResult", [
  Define.AutoField("userId", Nullable Int)
])
and CheckSessionResult = Define.Object<CheckSessionResult>("CheckSessionResult", [
  Define.AutoField("id", Int)
  Define.AutoField("name", String)
  Define.AutoField("email", String)
  Define.AutoField("inboxSize", Int)
])
and RegisterUserResult = Define.Object<RegisterUserResult>("RegisterUserResult", [
  Define.AutoField("id", Int)
])
and BalanceToOtherUser = Define.Object<BalanceToOtherUser>("BalanceToOtherUser", [
  Define.AutoField("otherUserId", Int)
  Define.AutoField("value", Float)
  Define.AutoField("iHaveMore", Boolean)
  Define.AutoField("sharedPaymentCount", Int)
  Define.AutoField("transferCount", Int)
  Define.AutoField("authoredByMeCount", Int)
  Define.AutoField("authoredByOtherUserCount", Int)
  Define.AutoField("inboxForMeCount", Int)
  Define.AutoField("inboxForOtherUserCount", Int)
  Define.AutoField("lastUpdateAt", Date)
  Define.Field("otherUser", User, fun ctx balance -> (Repo.getUserById balance.otherUserId).Value)
])
and AcceptTransactionsResult = Define.Object<AcceptTransactionsResult>("AcceptTransactionsResult", [
  Define.AutoField("acceptedIds", ListOf Int)
  Define.AutoField("failedIds", ListOf Int)
])
and AllGraphqlTypes : NamedDef list =
  [ User
    UserTransaction
    SignInResult
    SignOutResult
    CheckSessionResult
    RegisterUserResult
    //Balance
    BalanceToOtherUser
    AcceptTransactionsResult
  ]

//////////////////////

type Session =
  { mutable authorizedUserId : int option
    mutable token : string option }

// TODO move helper
let tryFindField (fieldName : string) ctx =
  List.tryPick
    (fun sel -> match sel with | Ast.Field f -> if f.Name = fieldName then Some f else None)
    ctx.ExecutionInfo.Ast.SelectionSet

let Query = Define.Object<Ref<Session>>("query", [
  Define.Field("users", ListOf User, "All users", [],
    fun ctx session ->
      if (!session).authorizedUserId.IsNone then
        let exc = new UnauthorizedAccessException("you are not logged in")
        do ctx.AddError exc
        raise exc
      else
        Repo.getAllUsers()
  )
  Define.Field("user", Nullable User, "Specified user", [Define.Input ("id", Int)],
    fun ctx session -> Repo.getUserById <| ctx.Arg("id")
  )
  Define.Field("currentUser", Nullable User, "Get currently logged in user", [], fun ctx session ->
    (!session).authorizedUserId |> Option.bind Repo.getUserById)
])
  
let Mutation = Define.Object<Ref<Session>>("mutation", [
  Define.Field("addTransaction", UserTransaction,
    "Remember transaction between payor and beneficients, then update balance between them", [
      Define.Input("payorId", Int)
      Define.Input("beneficientIds", ListOf Int)
      Define.Input("amount", Int)
      Define.Input("tags", ListOf String)
      Define.Input("description", Nullable String)
    ], fun ctx session ->
    let args : Input_AddTransaction = {
      authorId = (!session).authorizedUserId |> Option.defaultValue 0
      payorId = ctx.Arg "payorId"
      beneficientIds = ctx.Arg "beneficientIds"
      amount = ctx.Arg "amount"
      tags = ctx.Arg "tags"
      description = ctx.TryArg "description"
    }
    
    let result = Repo.addTransaction args
    match result with
    | Ok transaction ->
        do Repo.updateBalanceForUsers (args.payorId :: args.authorId :: args.beneficientIds) |> ignore
        transaction
    | Error err -> failwith err
  )
  
  Define.Field("registerUser", RegisterUserResult, "Register a new user", [
    Define.Input("email", String)
    Define.Input("name", String)
    Define.Input("passwordHash", String)
  ], fun ctx session ->
    let passwordSalted = Md5.hash <| (ctx.Arg "passwordHash") + AppConfig.Auth.passwordSalt
    Repo.addUser (ctx.Arg "email") passwordSalted (ctx.Arg "name")
      |> Option.map (fun id -> {id = id})
      |> Option.orDefault (fun () -> failwith "couldn't add user")
  )
  
  Define.Field("signIn", SignInResult, "Sign in user of given email with a password", [
    Define.Input("email", String)
    Define.Input("passwordHash", String, description = "md5 hashed password where salt (added on the right) is login email")
  ], fun ctx session ->
    let email = ctx.Arg "email"
    let passwordSalted = Md5.hash <| (ctx.Arg "passwordHash") + AppConfig.Auth.passwordSalt

    Repo.validateUserCredentials email passwordSalted
    |> Option.map (fun user ->
      // a little dirty - 'do' in '.map'
      do
        session := {
          token = Some <| createToken user.id
          authorizedUserId = Some user.id }

      let balances = Repo.getUserBalances user.id
      let inboxSize = List.sumBy (fun b -> b.inboxForMeCount) balances

      { SignInResult.id = user.id
        email = user.email
        name = user.name
        inboxSize = inboxSize } )
    |> Option.orDefault (fun () -> failwith "can not log in")
  )
  
  Define.Field("signOut", SignOutResult, "Sign out current user", [], fun ctx session ->
    let ret = { userId = (!session).authorizedUserId }
    
    do session := {
      token = None
      authorizedUserId = None }
    // TODO fix: cookie is not unset here!
    ret
  )
  
  Define.Field("checkSession", Nullable CheckSessionResult, "Check who is signed in based on httponly safe cookies", [], fun ctx session ->
    (!session).authorizedUserId
    |> Option.map Repo.getUserById
    |> Option.flatten
    |> Option.map (fun user ->
      let balances = Repo.getUserBalances user.id
      let inboxSize = List.sumBy (fun b -> b.inboxForMeCount) balances
      { id = user.id
        name = user.name
        email = user.email
        inboxSize = inboxSize } )
  )
  
  Define.Field("acceptTransactions", Nullable AcceptTransactionsResult, "Accept transactions added by someone else", [
    Define.Input("transactionIds", ListOf Int)
  ], fun ctx session ->
    match (!session).authorizedUserId with
    | Some userId ->
      let tids : int list = ctx.Arg "transactionIds"
      let updatedIds = Repo.acceptTransactions userId tids      
      let failedIds = Set.toList <| Set.difference (Set.ofList tids) (Set.ofList updatedIds)

      Some { acceptedIds = updatedIds
             failedIds = failedIds }

    | None ->
      failwith "not authenticated"
  )
])
