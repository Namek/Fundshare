module Page.TransactionList exposing (Model, Msg, init, update, view)

import Cmd.Extra
import Data.Context exposing (ContextData, GlobalMsg, Logged)
import Data.Session exposing (Session)
import Data.Transaction exposing (Transaction)
import Element exposing (Element, row)
import Json.Decode as Json
import Request.Common exposing (..)
import Request.Transactions exposing (requestUserTransactions)
import Task
import Time exposing (Posix, now)



-- MODEL --


type alias Model =
    {}


init : Session -> ( Model, Cmd Msg )
init session =
    ( {}
    , Cmd.batch
        [ Cmd.Extra.perform RefreshTransactions
        , Task.attempt SetDate Time.now
        ]
    )


type alias Context msg =
    Logged (ContextData Model Msg msg)



-- VIEW --


view : Context msg -> Element msg
view { model } =
    row [] [ viewHeader "Inbox" ]


viewHeader : String -> Element msg
viewHeader str =
    Element.paragraph [] [ Element.text str ]



-- UPDATE --


type Msg
    = RefreshTransactions
    | RefreshTransactionsResponse (Result Error (List Transaction))
    | SetDate (Result String Posix)


update : Context msg -> Msg -> ( ( Model, Cmd Msg ), Cmd GlobalMsg )
update { model, session } msg =
    case msg of
        RefreshTransactions ->
            let
                cmd =
                    requestUserTransactions
                        |> sendQueryRequest
                        |> Task.attempt RefreshTransactionsResponse
            in
            ( ( model, cmd ), Cmd.none )

        RefreshTransactionsResponse err ->
            ( ( model, Cmd.none ), Cmd.none )

        SetDate dateStringResult ->
            ( ( model, Cmd.none ), Cmd.none )
