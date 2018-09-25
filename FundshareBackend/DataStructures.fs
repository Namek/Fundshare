module Fundshare.DataStructures

open System


type User = {
  id : int
  email : string
  name : string
  balances : (BalanceToOtherUser list) option
  transactions : (UserTransaction list) option
}
and Transaction = {
  id : int
  payorId : int
  beneficientIds : int list
  amount : float
  description : string option
  tags : string list
}
and UserTransaction = {
  id : int
  payorId : int
  beneficientIds : int list
  amount : int
  description : string option
  tags : string list
  insertedAt : DateTime
}
and Input_AddTransaction = {
  payorId : int
  beneficientIds : int list
  amount : int
  tags : string list
  description : string option
}
and Balance = {
  user1Id : int
  user2Id : int
  balanceNum : int
  balanceDen : int
  user1HasMore : bool
  sharedPaymentCount : int
  transferCount : int
  unseenUpdateCount : int
  lastUpdateAt : DateTime
}
and BalanceToOtherUser = {
  otherUserId : int
  value : float
  iHaveMore : bool
  sharedPaymentCount : int
  transferCount : int
  unseenUpdateCount : int
  lastUpdateAt : DateTime
  otherUser : User option
}