module Request.Balance exposing (getBalances)

import Data.Balance exposing (Balance)
import GraphQL.Request.Builder exposing (..)
import Request.Common exposing (date)
import Time exposing (Posix)


{-| The exact call is:

    currentUser currentUserBalances {
      balances {
        otherUser {
          userId
          name
        }
        value
        iHaveMore
        sharedPaymentCount
        transferCount
        unseenForMeCount
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
                |> with (field "unseenForMeCount" [] int)
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
    , unseenForMeCount : Int
    , lastUpdateAt : Maybe Posix
    }


type alias QOtherUser =
    { userId : Int
    , name : String
    }


queryToBalance : QBalance -> Balance
queryToBalance b =
    Balance b.otherUser.userId b.otherUser.name b.value b.iHaveMore b.sharedPaymentCount b.transferCount b.unseenForMeCount b.lastUpdateAt
