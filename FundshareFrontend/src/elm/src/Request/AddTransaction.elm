module Request.AddTransaction exposing (..)

import Data.Person exposing (PersonId)
import Data.Transaction exposing (TransactionId)
import GraphQL.Request.Builder exposing (..)
import GraphQL.Request.Builder.Arg as Arg
import GraphQL.Request.Builder.Variable as Var


addTransaction : NewTransaction -> Request Mutation TransactionId
addTransaction newTransaction =
    let
        amountVar =
            Var.required "amount" .amount Var.int

        payorIdVar =
            Var.required "payorId" .payorId Var.int

        payeeIdsVar =
            Var.required "payeeIds" .payeeIds (Var.list Var.int)

        paidAtVar =
            Var.optional "paidAt" .paidAt Var.string ""

        descriptionVar =
            Var.optional "description" .description Var.string ""

        tagsVar =
            Var.optional "tags" .tags (Var.list Var.string) []
    in
    extract
        (field "addTransaction"
            [ ( "amount", Arg.variable amountVar )
            , ( "payorId", Arg.variable payorIdVar )
            , ( "payeeIds", Arg.variable payeeIdsVar )

            -- , ( "paidAt", Arg.variable paidAtVar )
            , ( "description", Arg.variable descriptionVar )
            , ( "tags", Arg.variable tagsVar )
            ]
            (extract (field "id" [] int))
        )
        |> mutationDocument
        |> request newTransaction


type alias NewTransaction =
    { amount : Int
    , payorId : PersonId
    , payeeIds : List PersonId
    , description : Maybe String
    , tags : Maybe (List String)
    }
