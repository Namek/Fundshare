-- Do not manually edit this file, it was auto-generated by dillonkearns/elm-graphql
-- https://github.com/dillonkearns/elm-graphql


module Api.Object.UserTransaction exposing (acceptanceIds, amount, beneficientIds, beneficients, description, id, insertedAt, payorId, selection, tags)

import Api.InputObject
import Api.Interface
import Api.Object
import Api.Scalar
import Api.Union
import Graphql.Field as Field exposing (Field)
import Graphql.Internal.Builder.Argument as Argument exposing (Argument)
import Graphql.Internal.Builder.Object as Object
import Graphql.Internal.Encode as Encode exposing (Value)
import Graphql.OptionalArgument exposing (OptionalArgument(..))
import Graphql.SelectionSet exposing (SelectionSet)
import Json.Decode as Decode


{-| Select fields to build up a SelectionSet for this object.
-}
selection : (a -> constructor) -> SelectionSet (a -> constructor) Api.Object.UserTransaction
selection constructor =
    Object.selection constructor


acceptanceIds : Field (List Int) Api.Object.UserTransaction
acceptanceIds =
    Object.fieldDecoder "acceptanceIds" [] (Decode.int |> Decode.list)


amount : Field Int Api.Object.UserTransaction
amount =
    Object.fieldDecoder "amount" [] Decode.int


beneficientIds : Field (List Int) Api.Object.UserTransaction
beneficientIds =
    Object.fieldDecoder "beneficientIds" [] (Decode.int |> Decode.list)


beneficients : SelectionSet decodesTo Api.Object.User -> Field (List decodesTo) Api.Object.UserTransaction
beneficients object_ =
    Object.selectionField "beneficients" [] object_ (identity >> Decode.list)


description : Field (Maybe String) Api.Object.UserTransaction
description =
    Object.fieldDecoder "description" [] (Decode.string |> Decode.nullable)


id : Field Int Api.Object.UserTransaction
id =
    Object.fieldDecoder "id" [] Decode.int


insertedAt : Field Api.Scalar.Date Api.Object.UserTransaction
insertedAt =
    Object.fieldDecoder "insertedAt" [] (Object.scalarDecoder |> Decode.map Api.Scalar.Date)


payorId : Field Int Api.Object.UserTransaction
payorId =
    Object.fieldDecoder "payorId" [] Decode.int


tags : Field (List String) Api.Object.UserTransaction
tags =
    Object.fieldDecoder "tags" [] (Decode.string |> Decode.list)
