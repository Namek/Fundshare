module Request.Common exposing (DateTimeType(..), Error, date, makeRequestOpts, sendMutationRequest, sendQueryRequest)

import Data.Session exposing (Session)
import GraphQL.Client.Http as GraphQLClient exposing (RequestOptions)
import GraphQL.Request.Builder exposing (..)
import Http
import ISO8601
import Json.Decode as Decode
import Task exposing (Task)
import Time exposing (Posix)


type alias Error =
    GraphQLClient.Error


makeRequestOpts : RequestOptions
makeRequestOpts =
    { method = "POST"
    , headers = []
    , url = "/api"
    , timeout = Nothing
    , withCredentials = False
    }


sendQueryRequest : Request Query a -> Task GraphQLClient.Error a
sendQueryRequest request =
    GraphQLClient.customSendQuery makeRequestOpts request


sendMutationRequest : Request Mutation a -> Task GraphQLClient.Error a
sendMutationRequest request =
    GraphQLClient.customSendMutation makeRequestOpts request


type DateTimeType
    = DateTimeType


date : ValueSpec NonNull DateTimeType Posix vars
date =
    Decode.string
        |> Decode.andThen
            (\str ->
                case ISO8601.fromString str of
                    Ok time ->
                        Decode.succeed (ISO8601.toPosix time)

                    Err errorMessage ->
                        Decode.fail errorMessage
            )
        |> customScalar DateTimeType
