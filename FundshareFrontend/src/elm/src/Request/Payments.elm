module Request.Payments exposing (PaymentFilter(..), getPayments)

import Data.Payment exposing (PaymentId)
import Date exposing (Date)
import GraphQL.Request.Builder exposing (..)
import Request.Common exposing (date)


type PaymentFilter
    = NeedingReview
    | ByTags (List String)
    | ByTextSearch String


{-| -}
getPayments : Request Query (List Payment)
getPayments =
    let
        _ =
            5
    in
    extract
        (field "currentUser"
            []
            (extract
                (field "payments"
                    []
                    (list (map queryToPayment payment))
                )
            )
        )
        |> namedQueryDocument "currentUserPayments"
        |> request {}
