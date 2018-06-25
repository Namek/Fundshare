module Request.Transactions exposing (..)

import Data.Transaction exposing (Transaction, TransactionId)
import Date exposing (Date)
import GraphQL.Request.Builder exposing (..)
import Json.Decode
import Request.Common exposing (date)


{-| Decoder for query:

    query {
      currentUser {
        transactions {
          amount
          description
          tags
          paidAt
          payeeIds
          payees {
            id
          }
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
                            |> with (field "amount" [] float)
                            |> with (field "description" [] (nullable string))
                            |> with (field "tags" [] (list string))
                            |> with (field "payorId" [] int)
                            |> with
                                (field "payees"
                                    []
                                    (list
                                        (extract (field "id" [] int))
                                    )
                                )
                            |> with (field "paidAt" [] date)
                        )
                    )
                )
            )
        )
        |> queryDocument
        |> request {}


