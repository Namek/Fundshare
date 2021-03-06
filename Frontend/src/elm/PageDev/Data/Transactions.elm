module PageDev.Data.Transactions exposing (transactionList, transactions)

import Data.Transaction exposing (Transaction)
import Request.Transactions exposing (TransactionList)
import Time exposing (Posix)


transactions : List Transaction
transactions =
    [ { id = 1
      , amount = 6000
      , description = Nothing
      , tags = [ "lunch" ]
      , payorId = 1
      , beneficientIds = [ 1, 2 ]
      , acceptanceIds = []
      , insertedAt = Time.millisToPosix 1561159353000
      }
    , { id = 2
      , amount = 9000
      , description = Nothing
      , tags = [ "lunch" ]
      , payorId = 2
      , beneficientIds = [ 1, 2 ]
      , acceptanceIds = []
      , insertedAt = Time.millisToPosix 1561245752000
      }
    , { id = 3
      , amount = 500
      , description = Just "car cleaning"
      , tags = [ "car" ]
      , payorId = 2
      , beneficientIds = [ 1, 2 ]
      , acceptanceIds = []
      , insertedAt = Time.millisToPosix 1561245782000
      }
    , { id = 4
      , amount = 500
      , description = Just "another car cleaning"
      , tags = [ "car" ]
      , payorId = 2
      , beneficientIds = [ 1, 2 ]
      , acceptanceIds = [ 1 ]
      , insertedAt = Time.millisToPosix 1561248787000
      }
    ]


transactionList : TransactionList
transactionList =
    { transactions = transactions
    , offset = 0
    , limit = List.length transactions
    }
