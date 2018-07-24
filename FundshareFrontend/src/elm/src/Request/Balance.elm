module Request.Balance exposing (getBalances)

import Data.Balance exposing (Balance)
import Date exposing (Date)
import GraphQL.Request.Builder exposing (..)
import Request.Common exposing (date)


{-| The exact call is:

    currentUser {
      balances {
        otherUser {
          userId
          name
        }
        value
        iHaveMore
        sharedPaymentCount
        transferCount
        unseenUpdateCount
        lastUpdateAt
      }
    }

-}
getBalances : Request Query (List Balance)
getBalances =
    let
        otherUser =
            object QOtherUser
                |> with (field "id" [] int)
                |> with (field "name" [] string)

        balance =
            object QBalance
                |> with (field "value" [] float)
                |> with (field "iHaveMore" [] bool)
                |> with (field "otherUser" [] otherUser)
                |> with (field "sharedPaymentCount" [] int)
                |> with (field "transferCount" [] int)
                |> with (field "unseenUpdateCount" [] int)
                |> with (field "lastUpdateAt" [] (nullable date))
    in
    extract
        (field "currentUser"
            []
            (extract
                (field "balances"
                    []
                    (list (map queryToBalance balance))
                )
            )
        )
        |> namedQueryDocument "currentUserBalances"
        |> request {}



-- INTERNALS --


type alias QBalance =
    { value : Float
    , iHaveMore : Bool
    , otherUser : QOtherUser
    , sharedPaymentCount : Int
    , transferCount : Int
    , unseenUpdateCount : Int
    , lastUpdateAt : Maybe Date
    }


type alias QOtherUser =
    { userId : Int
    , name : String
    }


queryToBalance : QBalance -> Balance
queryToBalance b =
    Balance b.otherUser.userId b.otherUser.name b.value b.iHaveMore b.sharedPaymentCount b.transferCount b.unseenUpdateCount b.lastUpdateAt
