module Request.Transactions exposing (requestUserTransactions)

import Data.Transaction exposing (Transaction, TransactionId)
import GraphQL.Request.Builder exposing (..)
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
