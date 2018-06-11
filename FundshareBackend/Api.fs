module Fundshare.Api

open FSharp.Data.GraphQL
open FSharp.Data.GraphQL.Types
open FSharp.Data.GraphQL.Execution

open Fundshare.DataStructures

type SignInResult =
  { userId : int
    name : string
    token : string }
  
type RegisterUserResult =
  { id : int }
  
  
/////////////////////////

let rec User = Define.Object<User>("User", [
  Define.AutoField("id", Int)
  Define.AutoField("email", String)
  Define.AutoField("name", String)
  // balances
  // transactions
])
  
and UserTransaction = Define.Object<UserTransaction>("UserTransaction", [
  Define.AutoField("id", Int)
])
  
and SignInResult = Define.Object<SignInResult>("SignInResult", [
  Define.AutoField("userId", Int)
  Define.AutoField("name", String)
  Define.AutoField("token", String)
])

and RegisterUserResult = Define.Object<RegisterUserResult>("RegisterUserResult", [
  Define.AutoField("id", Int)
])

//and Node = Define.Interface<obj>(fun () -> [ User; UserTransaction ])

let AllGraphqlTypes : NamedDef list = [ User; UserTransaction; SignInResult; RegisterUserResult ]

//////////////////////

  
let Query = Define.Object("Query", [
  Define.Field("users", ListOf User, "All users", [], fun ctx () -> [])
  Define.Field("user", Nullable User, "Specified user", [Define.Input ("id", Int)],
    fun ctx () -> ctx.Arg("id") |> Repo.getUserById)
//    Define.Field("currentUser", Nullable User, "Get currently logged in user", [], Session...) TODO 
])
  
let Mutation = Define.Object("Mutation", [
  Define.Field("addTransaction", Nullable UserTransaction,
    "Remember transaction between payor and payees, then update balance between them", [
      Define.Input("payorId", Int)
      Define.Input("payeeIds", ListOf Int)
      Define.Input("amount", Int)
      Define.Input("tags", ListOf String)
      Define.Input("description", Nullable String)
    ], fun ctx () ->
    let args : Input_AddTransaction = {
      payorId = ctx.Arg "payorId"
      payeeIds = ctx.Arg "payeeIds"
      amount = ctx.Arg "amount"
      tags = ctx.Arg "tags"
      description = ctx.TryArg "description"
    }
    
    Repo.addTransaction args
  )
  
  Define.Field("registerUser", RegisterUserResult, "Register a new user", [
    Define.Input("email", String)
    Define.Input("name", String)
    Define.Input("password", String)
  ], fun ctx () ->
    { id = 1 } // TODO register new user
  )
  
  Define.Field("signIn", Nullable SignInResult, "Sign in user of given email with a password", [
    Define.Input("email", String)
    Define.Input("password", String)
  ], fun ctx () ->
    Repo.validateUserCredentials (ctx.Arg "email") (ctx.Arg "password")
    |> Option.map (fun user ->
      
      {userId = user.id; name = user.name; token = "asd"})
  )
])
