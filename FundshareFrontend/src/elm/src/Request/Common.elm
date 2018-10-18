module Request.Common exposing
    ( decodeDate
    , sendMutationRequest
    , sendQueryRequest
    )

import Api.Scalar
import Data.Session exposing (Session)
import Graphql.Http as Http
import Graphql.Operation exposing (RootMutation, RootQuery)
import Graphql.SelectionSet as SelectionSet exposing (SelectionSet)
import ISO8601
import Json.Decode as Decode
import RemoteData exposing (RemoteData)
import Task exposing (Task)
import Time exposing (Posix)


graphQlApiUrl =
    "/api"


sendQueryRequest : (RemoteData (Http.Error response) response -> msg) -> SelectionSet response RootQuery -> Cmd msg
sendQueryRequest dataHandlerMsg query =
    query
        |> Http.queryRequest graphQlApiUrl
        |> Http.send (RemoteData.fromResult >> dataHandlerMsg)


sendMutationRequest : (RemoteData (Http.Error response) response -> msg) -> SelectionSet response RootMutation -> Cmd msg
sendMutationRequest dataHandlerMsg query =
    query
        |> Http.mutationRequest graphQlApiUrl
        |> Http.send (RemoteData.fromResult >> dataHandlerMsg)


decodeDate : Api.Scalar.Date -> Posix
decodeDate (Api.Scalar.Date str) =
    case ISO8601.fromString str of
        Ok time ->
            ISO8601.toPosix time

        Err errorMessage ->
            Debug.todo "omg"
