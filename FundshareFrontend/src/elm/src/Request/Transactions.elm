module Request.Transactions exposing
    ( AcceptTransactionsResult
    , TransactionList
    , acceptTransactions
    , getUserTransactions
    )

import Api.Mutation as Mutation
import Api.Object.AcceptTransactionsResult as AcceptTransactionsResult
import Api.Object.User as User
import Api.Object.UserTransaction as UserTransaction
import Api.Query as Query
import Api.Scalar
import Data.Transaction exposing (Transaction, TransactionId)
import Graphql.Field as Field
import Graphql.Operation exposing (RootMutation, RootQuery)
import Graphql.OptionalArgument exposing (OptionalArgument(..))
import Graphql.SelectionSet as SelectionSet exposing (SelectionSet, with)
import RemoteData exposing (RemoteData)
import Request.Common exposing (decodeDate)


type alias TransactionList =
    { transactions : List Transaction
    , offset : Int
    , limit : Int
    }


type alias AcceptTransactionsResult =
    { acceptedIds : List Int
    , failedIds : List Int
    }


{-| Query:

    query currentUserTransactions {
      currentUser {
        transactions {
          id
          amount
          description
          tags
          beneficientIds
          acceptanceIds
          insertedAt
        }
      }
    }

-}
getUserTransactions : Int -> Int -> SelectionSet TransactionList RootQuery
getUserTransactions offset limit =
    let
        params =
            { offset = Present offset
            , limit = Present limit
            }

        defaultResult =
            { offset = offset, limit = 0, transactions = [] }
    in
    Query.selection (Maybe.withDefault defaultResult)
        |> with
            (Query.currentUser
                (User.selection identity
                    |> with
                        (User.transactions (always params) transaction
                            |> Field.map (\t -> { defaultResult | limit = limit, transactions = t })
                        )
                )
            )


transaction =
    UserTransaction.selection Transaction
        |> with UserTransaction.id
        |> with UserTransaction.amount
        |> with UserTransaction.description
        |> with UserTransaction.tags
        |> with UserTransaction.payorId
        |> with UserTransaction.beneficientIds
        |> with UserTransaction.acceptanceIds
        |> with (UserTransaction.insertedAt |> Field.map decodeDate)


acceptTransactions : List TransactionId -> SelectionSet AcceptTransactionsResult RootMutation
acceptTransactions transactionIds =
    Mutation.selection (Maybe.withDefault { acceptedIds = [], failedIds = transactionIds })
        |> with
            (Mutation.acceptTransactions { transactionIds = transactionIds }
                (AcceptTransactionsResult.selection AcceptTransactionsResult
                    |> with AcceptTransactionsResult.acceptedIds
                    |> with AcceptTransactionsResult.failedIds
                )
            )
