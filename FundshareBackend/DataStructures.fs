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

type UserTransaction = {
  id : int
  payeeIds : int list
  amount : int
  description : string option
  tags : string list
  paidAt : DateTime
}

type Input_AddTransaction = {
  payorId : int
  payeeIds : int list
  amount : int
  tags : string list
  description : string option
  paidAt : DateTime option
}



//type AddTransactionInput = {
//  
//}