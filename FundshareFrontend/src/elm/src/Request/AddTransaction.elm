module Request.AddTransaction exposing (NewTransaction, addTransaction)

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

        beneficientIdsVar =
            Var.required "beneficientIds" .beneficientIds (Var.list Var.int)

        insertedAtVar =
            Var.optional "insertedAt" .insertedAt Var.string ""

        descriptionVar =
            Var.optional "description" .description Var.string ""

        tagsVar =
            Var.optional "tags" .tags (Var.list Var.string) []
    in
    extract
        (field "addTransaction"
            [ ( "amount", Arg.variable amountVar )
            , ( "payorId", Arg.variable payorIdVar )
            , ( "beneficientIds", Arg.variable beneficientIdsVar )

            -- , ( "insertedAt", Arg.variable insertedAtVar )
            , ( "description", Arg.variable descriptionVar )
            , ( "tags", Arg.variable tagsVar )
            ]
            (extract (field "id" [] int))
        )
        |> namedMutationDocument "addTransaction"
        |> request newTransaction


type alias NewTransaction =
    { amount : Int
    , payorId : PersonId
    , beneficientIds : List PersonId
    , description : Maybe String
    , tags : Maybe (List String)
    }
