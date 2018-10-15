module Request.Transactions exposing (AcceptTransactions, acceptTransactions, requestUserTransactions)

import Data.Transaction exposing (Transaction, TransactionId)
import GraphQL.Request.Builder exposing (..)
import GraphQL.Request.Builder.Arg as Arg
import GraphQL.Request.Builder.Variable as Var
import Json.Decode
import Request.Common exposing (date)


{-| Decoder for query:

    query currentUserTransactions {
      currentUser {
        transactions {
          amount
          description
          tags
          beneficients {
            userId
          }
          acceptanceIds
          insertedAt
        }
      }
    }

    -- TODO filter for unseen by user

-}
requestUserTransactions : Request Query (List Transaction)
requestUserTransactions =
    extract
        (field "currentUser"
            []
            (extract
                (field "transactions"
                    []
                    (list <|
                        (object Transaction
                            |> with (field "id" [] int)
                            |> with (field "amount" [] int)
                            |> with (field "description" [] (nullable string))
                            |> with (field "tags" [] (list string))
                            |> with (field "payorId" [] int)
                            |> with
                                (field "beneficients"
                                    []
                                    (list
                                        (extract (field "id" [] int))
                                    )
                                )
                            |> with (field "acceptanceIds" [] (list int))
                            |> with (field "insertedAt" [] date)
                        )
                    )
                )
            )
        )
        |> namedQueryDocument "currentUserTransactions"
        |> request {}


acceptTransactions : List TransactionId -> Request Mutation AcceptTransactions
acceptTransactions transactionIds =
    let
        tidsVar =
            Var.required "transactionIds" identity (Var.list Var.int)
    in
    extract
        (field "acceptTransactions"
            [ ( "transactionIds", Arg.variable tidsVar ) ]
            (object AcceptTransactions
                |> with (field "acceptedIds" [] (list int))
                |> with (field "failedIds" [] (list int))
            )
        )
        |> namedMutationDocument "AcceptTransactions"
        |> request transactionIds


type alias AcceptTransactions =
    { acceptedIds : List TransactionId
    , failedIds : List TransactionId
    }
