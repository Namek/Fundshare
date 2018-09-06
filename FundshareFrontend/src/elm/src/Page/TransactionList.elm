module Page.TransactionList exposing (Model, Msg, init, update, view)

import Cmd.Extra
import Data.Context exposing (ContextData, GlobalMsg, Logged)
import Data.Session exposing (Session)
import Data.Transaction exposing (Transaction)
import Date exposing (Date, now)
import Date.Distance
import Html exposing (Html, div, p, text)
import Html.Events
import Json.Decode as Json
import Material.Button as Button
import Material.Elevation as Elevation
import Material.Options as Options exposing (css, when)
import Material.Progress as Loading
import Material.Textfield as Textfield
import Material.Typography as Typo
import Misc exposing ((=>))
import Request.Common exposing (..)
import Request.Transactions exposing (requestUserTransactions)
import Task



-- MODEL --


type alias Model =
    {}


init : Session -> ( Model, Cmd Msg )
init session =
    {}
        => Cmd.batch
            [ Cmd.Extra.perform RefreshTransactions
            , Task.attempt SetDate Date.now
            ]


type alias Context msg =
    Logged (ContextData Model Msg msg)



-- VIEW --


view : Context msg -> Html msg
view { model } =
    div []
        [ viewHeader "Inbox"
        ]


viewHeader : String -> Html msg
viewHeader str =
    Options.styled p [ Typo.headline ] [ text str ]



-- UPDATE --


type Msg
    = RefreshTransactions
    | RefreshTransactionsResponse (Result Error (List Transaction))
    | SetDate (Result String Date)


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
            model => cmd => Cmd.none

        RefreshTransactionsResponse err ->
            model => Cmd.none => Cmd.none

        SetDate dateStringResult ->
            model => Cmd.none => Cmd.none
