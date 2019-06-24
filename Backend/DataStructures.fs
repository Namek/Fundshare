module Backend.DataStructures

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
  authorId : int
  payorId : int
  beneficientIds : int list
  acceptanceIds : int list
  amount : float
  description : string option
  tags : string list
}
and UserTransaction = {
  id : int
  authorId : int
  payorId : int
  beneficientIds : int list
  acceptanceIds : int list
  amount : int
  description : string option
  tags : string list
  insertedAt : DateTime
  beneficients : (User list) option
}
and Input_AddTransaction = {
  authorId : int
  payorId : int
  beneficientIds : int list
  amount : int
  tags : string list
  description : string option
}
and Input_EditTransaction = {
  payorId : int option
  beneficientIds : (int list) option
  amount : int option
  tags : (string list) option
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
  authoredByUser1Count : int
  authoredByUser2Count : int
  inboxForMeCount : int
  inboxForOtherUserCount : int
  lastUpdateAt : DateTime
}
and BalanceToOtherUser = {
  otherUserId : int
  value : float
  iHaveMore : bool
  sharedPaymentCount : int
  transferCount : int
  authoredByMeCount : int
  authoredByOtherUserCount : int
  inboxForMeCount : int
  inboxForOtherUserCount : int
  lastUpdateAt : DateTime
  otherUser : User option
}
and AcceptTransactionsResult = {
  acceptedIds : int list
  failedIds : int list
}