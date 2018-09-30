module Page.TransactionList exposing (Model, Msg, init, update, view)

import Cmd.Extra
import Data.Context exposing (ContextData, GlobalMsg, Logged)
import Data.Person exposing (PersonId)
import Data.Session exposing (Session)
import Data.Transaction exposing (Transaction, TransactionId, amountDifferenceForMyAccount, amountToMoney, isTransactionUnseenForUser)
import Element exposing (Element, column, el, fill, paragraph, px, row, shrink, spacing, table, text)
import Element.Font as Font
import Element.Input as Input
import Json.Decode as Json
import Misc exposing (either, noCmd)
import Request.Common exposing (..)
import Request.Transactions exposing (requestUserTransactions)
import Task
import Time exposing (Posix, now)



-- MODEL --


type alias Model =
    { transactions : List Transaction }


init : Session -> ( Model, Cmd Msg )
init session =
    ( { transactions = [] }
    , Cmd.batch
        [ Cmd.Extra.perform RefreshTransactions
        , Task.attempt SetDate Time.now
        ]
    )


type alias Context msg =
    Logged (ContextData Model Msg msg)


hasAnyNewTransaction : PersonId -> List Transaction -> Bool
hasAnyNewTransaction userId transactions =
    transactions |> List.any (isTransactionUnseenForUser userId)



-- UPDATE --


type Msg
    = RefreshTransactions
    | RefreshTransactionsResponse (Result Error (List Transaction))
    | SetDate (Result String Posix)
    | AcceptTransaction TransactionId


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

        RefreshTransactionsResponse (Err err) ->
            ( ( model, Cmd.none ), Cmd.none )

        RefreshTransactionsResponse (Ok transactions) ->
            { model | transactions = transactions }
                |> noCmd
                |> noCmd

        SetDate dateStringResult ->
            ( ( model, Cmd.none ), Cmd.none )

        AcceptTransaction tid ->
            --TODO
            model |> noCmd |> noCmd



-- VIEW --


view : Context msg -> Element msg
view ctx =
    let
        { model, session } =
            ctx
    in
    column []
        [ hasAnyNewTransaction session.user.id model.transactions
            |> either (viewInbox ctx) Element.none
        ]


viewInbox : Context msg -> Element msg
viewInbox ctx =
    let
        { model, session } =
            ctx
    in
    column [ spacing 15 ]
        [ viewHeader <| "Inbox (" ++ (List.length model.transactions |> String.fromInt) ++ ")"
        , table [ Font.size 14, spacing 8 ]
            { data = model.transactions
            , columns =
                [ { header = text "check all"
                  , width = shrink
                  , view = \t -> text "chck"
                  }
                , { header = el [ Font.bold ] <| text "Change"
                  , width = px 100
                  , view =
                        \t ->
                            text
                                (amountDifferenceForMyAccount session.user.id t
                                    |> amountToMoney
                                    |> String.fromFloat
                                )
                  }
                , { header = el [ Font.bold ] <| text "Amount"
                  , width = px 100
                  , view = \t -> text (t.amount |> amountToMoney |> String.fromFloat)
                  }
                , { header = el [ Font.bold ] <| text "Tags"
                  , width = fill
                  , view = \t -> text (String.join ", " t.tags)
                  }
                ]
            }
        ]


viewHeader : String -> Element msg
viewHeader str =
    Element.paragraph [] [ Element.text str ]
