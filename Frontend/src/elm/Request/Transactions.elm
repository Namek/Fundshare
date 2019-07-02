module Request.Transactions exposing
    ( AcceptTransactionsResult
    , TransactionList
    , acceptTransactions
    , getUserMailboxTransactions
    , getUserTransactions
    , transaction
    )

import Api.Mutation as Mutation
import Api.Object.AcceptTransactionsResult as AcceptTransactionsResult
import Api.Object.User as User
import Api.Object.UserTransaction as UserTransaction
import Api.Query as Query
import Data.Transaction exposing (Transaction, TransactionId)
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
    SelectionSet.succeed (Maybe.withDefault defaultResult)
        |> with
            (Query.currentUser
                (User.transactions (always params) transaction
                    |> SelectionSet.map (\t -> { defaultResult | limit = limit, transactions = t })
                )
            )


{-| Query:

    query currentUserTransactions {
      currentUser {
        inboxTransactions {
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
getUserMailboxTransactions : SelectionSet TransactionList RootQuery
getUserMailboxTransactions =
    let
        params =
            { offset = Absent
            , limit = Absent
            }

        defaultResult =
            { offset = 0, limit = 0, transactions = [] }
    in
    SelectionSet.succeed (Maybe.withDefault defaultResult)
        |> with
            (Query.currentUser
                (User.mailboxTransactions (always params) transaction
                    |> SelectionSet.map (\t -> { defaultResult | transactions = t, limit = List.length t + 1 })
                )
            )


transaction =
    SelectionSet.succeed Transaction
        |> with UserTransaction.id
        |> with UserTransaction.amount
        |> with UserTransaction.description
        |> with UserTransaction.tags
        |> with UserTransaction.payorId
        |> with UserTransaction.beneficientIds
        |> with UserTransaction.acceptanceIds
        |> with (UserTransaction.insertedAt |> SelectionSet.map decodeDate)


acceptTransactions : List TransactionId -> SelectionSet AcceptTransactionsResult RootMutation
acceptTransactions transactionIds =
    SelectionSet.succeed (Maybe.withDefault { acceptedIds = [], failedIds = transactionIds })
        |> with
            (Mutation.acceptTransactions { transactionIds = transactionIds }
                (SelectionSet.succeed AcceptTransactionsResult
                    |> with AcceptTransactionsResult.acceptedIds
                    |> with AcceptTransactionsResult.failedIds
                )
            )
