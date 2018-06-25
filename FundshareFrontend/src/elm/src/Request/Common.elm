module Request.Common exposing (..)

import Data.Session exposing (Session)
import Date exposing (Date)
import GraphQL.Client.Http as GraphQLClient exposing (RequestOptions)
import GraphQL.Request.Builder exposing (..)
import Http
import Json.Decode as Decode
import Task exposing (Task)


type alias Error =
    GraphQLClient.Error


authorizedWith : Session -> RequestOptions
authorizedWith { authToken } =
    Just authToken |> makeRequestOpts


makeRequestOpts : Maybe String -> RequestOptions
makeRequestOpts authToken =
    let
        headers =
            authToken
                |> Maybe.andThen (\token -> Just [ Http.header "authorization" ("Bearer " ++ token) ])
                |> Maybe.withDefault []
    in
    { method = "POST"
    , headers = headers
    , url = "/api"
    , timeout = Nothing
    , withCredentials = False
    }


sendQueryRequest : { session | authToken : String } -> Request Query a -> Task GraphQLClient.Error a
sendQueryRequest { authToken } request =
    GraphQLClient.customSendQuery (Just authToken |> makeRequestOpts) request


sendMutationRequest : Maybe { session | authToken : String } -> Request Mutation a -> Task GraphQLClient.Error a
sendMutationRequest maybeSession request =
    GraphQLClient.customSendMutation (maybeSession |> Maybe.andThen (Just << .authToken) |> makeRequestOpts) request


type DateTimeType
    = DateTimeType


date : ValueSpec NonNull DateTimeType Date vars
date =
    Decode.string
        |> Decode.andThen
            (\str ->
                case Date.fromString str of
                    Ok time ->
                        Decode.succeed time

                    Err errorMessage ->
                        Decode.fail errorMessage
            )
        |> customScalar DateTimeType
