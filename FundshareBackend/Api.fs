module Fundshare.Api

open FSharp.Data.GraphQL
open FSharp.Data.GraphQL.Types

open Fundshare.DataStructures
//open Fundshare.Repo


let UserType = Define.Object<User>(
  "User", [
    Define.AutoField("id", Int)
    Define.AutoField("email", String)
    Define.AutoField("name", String)
    // balances
    // transactions
  ])
//
//Define.Field("users", ListOf UserType, "All users", [], fun ctx db ->
//  query {
////    for user in db.users do
////      select user
//  } |> Seq.toList)
  
  
let UserTransactionType = Define.Object<UserTransaction>("UserTransaction", [
  Define.AutoField("id", Int)
])
  
let Query = Define.Object("Query", [
  Define.Field("users", ListOf UserType, "All users", [], fun ctx () -> [])
  Define.Field("user", Nullable UserType, "Specified user", [Define.Input ("id", Int)],
      fun ctx () -> ctx.Arg("id") |> Repo.getUserById)
//    Define.Field("currentUser", Nullable UserType, "Get currently logged in user", [], Session...) TODO 
])
  
let Mutation = Define.Object("Mutation", [
  Define.Field("addTransaction", Nullable UserTransactionType, "Add new transaction", [
      Define.Input("payorId", Int)
      Define.Input("payeeIds", ListOf Int)
      Define.Input("amount", Int)
      Define.Input("tags", ListOf String)
      Define.Input("description", Nullable String)
      Define.Input("paidAt", Nullable Date)
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
])
  


//let schema = Schema(Query)