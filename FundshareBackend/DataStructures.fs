module Fundshare.DataStructures

open System


type User = {
  id : int
  email : string
  name : string
//  password_hash : string
//  inserted_at : int64
//  updated_at : int64
}

type TransactionType = Shared | Transfer

type UserTransaction = {
  id : int
  payorId : int
  payeeIds : int list
  amount : int
  description : string option
  tags : string list
  insertedAt : DateTime
}

type Input_AddTransaction = {
  payorId : int
  payeeIds : int list
  amount : int
  tags : string list
  description : string option
}

let decodeTransactionType (t : string) : TransactionType =
  match t.ToLower() with 
  | "shared" -> Shared
  | "transfer" -> Transfer
  | _ -> failwith <| "wrong transaction type: " + t
