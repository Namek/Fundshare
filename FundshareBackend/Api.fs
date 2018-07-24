module Fundshare.Api

open System
open System.Text
open FSharp.Data.GraphQL
open FSharp.Data.GraphQL.Parser
open FSharp.Data.GraphQL.Types
open FSharp.Data.GraphQL.Execution
open FSharp.Data.GraphQL.Linq
open Paseto.Authentication
open Fundshare.DataStructures
open YoLo

type SignInResult =
  { userId : int
    name : string }

type SignOutResult =
  { userId : int option }
  
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
  Define.AutoField("id", ID)
  Define.AutoField("email", String)
  Define.AutoField("name", String)
  Define.Field("balances", ListOf BalanceToOtherUser, fun ctx user -> Repo.getUserBalances user.id )
  Define.Field("transactions", ListOf UserTransaction)
])
  
and UserTransaction = Define.Object<UserTransaction>("UserTransaction", [
  Define.AutoField("id", Int)
])
and SignInResult = Define.Object<SignInResult>("SignInResult", [
  Define.AutoField("userId", Int)
  Define.AutoField("name", String)
])
and SignOutResult = Define.Object<SignOutResult>("SignOutResult", [
  Define.AutoField("userId", Nullable Int)
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
  Define.AutoField("unseenUpdateCount", Int)
  Define.AutoField("lastUpdateAt", Date)
  Define.Field("otherUser", User)
])
and AllGraphqlTypes : NamedDef list =
  [ User
    UserTransaction
    SignInResult
    SignOutResult
    RegisterUserResult
    //Balance
    BalanceToOtherUser
  ]

//////////////////////

type Session =
  { authorizedUserId : int option
    mutable token : string option }

// TODO move helper
let tryFindField (fieldName : string) ctx =
  List.tryPick
    (fun sel -> match sel with | Ast.Field f -> if f.Name = fieldName then Some f else None)
    ctx.ExecutionInfo.Ast.SelectionSet

let Query = Define.Object<Session>("query", [
  Define.Field("users", ListOf User, "All users", [], fun ctx _ -> [])
  Define.Field("user", Nullable User, "Specified user", [Define.Input ("id", Int)],
    fun ctx session ->
      let user = Repo.getUserById <| ctx.Arg("id") 
      user |> Option.map (fun user ->
        let transactions = 
          tryFindField "transactions" ctx
          |> Option.map (fun _ -> Repo.getUserTransactions user.id )

        { user with transactions = transactions }
      )
  )
  Define.Field("currentUser", Nullable User, "Get currently logged in user", [], fun ctx session ->
    session.authorizedUserId |> Option.bind Repo.getUserById)
])
  
let Mutation = Define.Object<Session>("mutation", [
  Define.Field("addTransaction", Nullable UserTransaction,
    "Remember transaction between payor and payees, then update balance between them", [
      Define.Input("payorId", Int)
      Define.Input("payeeIds", ListOf Int)
      Define.Input("amount", Int)
      Define.Input("tags", ListOf String)
      Define.Input("description", Nullable String)
    ], fun ctx session ->
    let args : Input_AddTransaction = {
      payorId = ctx.Arg "payorId"
      payeeIds = ctx.Arg "payeeIds"
      amount = ctx.Arg "amount"
      tags = ctx.Arg "tags"
      description = ctx.TryArg "description"
    }
    
    Repo.addTransaction args
  )
  
  Define.Field("registerUser", Nullable RegisterUserResult, "Register a new user", [
    Define.Input("email", String)
    Define.Input("name", String)
    Define.Input("passwordHash", String)
  ], fun ctx session ->
    let passwordSalted = UTF8.sha1Hex <| (ctx.Arg "passwordHash") + AppConfig.Auth.passwordSalt
    Repo.addUser (ctx.Arg "email") passwordSalted (ctx.Arg "name")
      |> Option.map (fun id -> {id = id})
  )
  
  Define.Field("signIn", Nullable SignInResult, "Sign in user of given email with a password", [
    Define.Input("email", String)
    Define.Input("passwordHash", String, description = "sha1 hashed password where salt (added on the right) is login email")
  ], fun ctx session ->
    let email = ctx.Arg "email"
    let passwordSalted = UTF8.sha1Hex <| (ctx.Arg "passwordHash") + AppConfig.Auth.passwordSalt
    
    Repo.validateUserCredentials email passwordSalted
    |> Option.map (fun user ->
      // a little dirty - 'do' in '.map'
      if session.token = None then do
        session.token <- Some <| createToken user.id

      { userId = user.id; name = user.name })
  )
  
  Define.Field("signOut", SignOutResult, "Sign out current user", [], fun ctx session ->
    do session.token <- None
    { userId = session.authorizedUserId }
  )
])
