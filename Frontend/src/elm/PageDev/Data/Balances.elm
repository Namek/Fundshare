module PageDev.Data.Balances exposing (balances)

import Data.Balance exposing (Balance)
import Time exposing (Posix)


balances : List Balance
balances =
    [ { personId = 1
      , name = "Christine"
      , value = 2563.62
      , iHaveMore = False
      , sharedPaymentCount = 23
      , transferCount = 16
      , inboxForMeCount = 10
      , lastUpdateAt = Time.millisToPosix 1561248787000
      }
    ]
