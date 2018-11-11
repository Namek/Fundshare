module Request.Balance exposing (getBalances)

import Api.Object
import Api.Object.BalanceToOtherUser as BalanceToOtherUser
import Api.Object.User as User
import Api.Query as Query
import Data.Balance exposing (Balance)
import Graphql.Field as Field
import Graphql.Internal.Builder.Object exposing (fieldDecoder)
import Graphql.Operation exposing (RootMutation, RootQuery)
import Graphql.SelectionSet as SelectionSet exposing (SelectionSet, with)
import Json.Decode as Decode
import Request.Common exposing (decodeDate)
import Time exposing (Posix)


{-| Query:

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
        inboxForMeCount
        lastUpdateAt
      }
    }

-}
getBalances : SelectionSet (List Balance) RootQuery
getBalances =
    Query.selection (Maybe.withDefault [])
        |> with
            (Query.currentUser
                (User.selection identity
                    |> with (User.balances userBalances)
                )
            )


userBalances : SelectionSet Balance Api.Object.BalanceToOtherUser
userBalances =
    BalanceToOtherUser.selection QBalance
        |> with BalanceToOtherUser.value
        |> with BalanceToOtherUser.iHaveMore
        |> with (BalanceToOtherUser.otherUser otherUser)
        |> with BalanceToOtherUser.sharedPaymentCount
        |> with BalanceToOtherUser.transferCount
        |> with BalanceToOtherUser.inboxForMeCount
        |> with (BalanceToOtherUser.lastUpdateAt |> Field.map decodeDate)
        |> SelectionSet.map queryToBalance


otherUser : SelectionSet QOtherUser Api.Object.User
otherUser =
    User.selection QOtherUser
        |> with (fieldDecoder "id" [] Decode.int)
        |> with User.name


type alias QBalance =
    { value : Float
    , iHaveMore : Bool
    , otherUser : QOtherUser
    , sharedPaymentCount : Int
    , transferCount : Int
    , inboxForMeCount : Int
    , lastUpdateAt : Posix
    }


type alias QOtherUser =
    { userId : Int
    , name : String
    }


queryToBalance : QBalance -> Balance
queryToBalance b =
    Balance b.otherUser.userId b.otherUser.name b.value b.iHaveMore b.sharedPaymentCount b.transferCount b.inboxForMeCount b.lastUpdateAt
