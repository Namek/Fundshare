module Page.TransactionList exposing (Model, Msg, init, update, view)

import Cmd.Extra
import Data.Context exposing (ContextData, GlobalMsg, Logged)
import Data.Person exposing (PersonId)
import Data.Session exposing (Session)
import Data.Transaction exposing (Transaction, TransactionId, amountDifferenceForMyAccount, amountToMoney, isTransactionUnseenForUser)
import Element exposing (Element, alignRight, centerY, column, el, fill, padding, paddingEach, paragraph, px, row, shrink, spacing, table, text, width)
import Element.Font as Font
import Element.Input as Input
import GraphQL.Client.Http
import Json.Decode as Json
import List.Extra
import Misc exposing (attrWhen, edges, either, noCmd, noShadow, styledButton, toggle, userSelectNone, viewIcon)
import Misc.Colors exposing (gray500)
import Request.Common exposing (..)
import Request.Transactions exposing (AcceptTransactions, acceptTransactions, requestUserTransactions)
import Set exposing (Set)
import Task
import Time exposing (Posix, now)



-- MODEL --


type alias Model =
    { inboxTransactions : List Transaction
    , selectedInboxTransactionIds : Set TransactionId
    }


init : Session -> ( Model, Cmd Msg )
init session =
    ( { inboxTransactions = []
      , selectedInboxTransactionIds = Set.empty
      }
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


allSelected model =
    List.length model.inboxTransactions == Set.size model.selectedInboxTransactionIds


someSelected model =
    not <| Set.isEmpty model.selectedInboxTransactionIds



-- UPDATE --


type Msg
    = RefreshTransactions
    | RefreshTransactionsResponse (Result Error (List Transaction))
    | SetDate (Result String Posix)
    | ToggleInboxTransaction TransactionId
    | ToggleCheckAllInboxTransactions
    | AcceptSelectedTransactions
    | AcceptSelectedTransactions_Response (Result GraphQL.Client.Http.Error AcceptTransactions)


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
            { model
                | inboxTransactions = List.filter (isTransactionUnseenForUser session.user.id) transactions
            }
                |> noCmd
                |> noCmd

        SetDate dateStringResult ->
            ( ( model, Cmd.none ), Cmd.none )

        ToggleInboxTransaction tid ->
            { model | selectedInboxTransactionIds = toggle tid model.selectedInboxTransactionIds }
                |> noCmd
                |> noCmd

        ToggleCheckAllInboxTransactions ->
            { model
                | selectedInboxTransactionIds =
                    if someSelected model then
                        Set.empty

                    else
                        model.inboxTransactions |> List.map .id |> Set.fromList
            }
                |> noCmd
                |> noCmd

        AcceptSelectedTransactions ->
            let
                cmd =
                    model.selectedInboxTransactionIds
                        |> Set.toList
                        |> acceptTransactions
                        |> sendMutationRequest
                        |> Task.attempt AcceptSelectedTransactions_Response
            in
            ( model, cmd ) |> noCmd

        AcceptSelectedTransactions_Response response ->
            case response of
                Ok data ->
                    let
                        acceptedIds =
                            Set.fromList data.acceptedIds

                        failedIds =
                            Set.fromList data.failedIds

                        shouldBeLeft tid =
                            (not <| Set.member tid acceptedIds)
                                || Set.member tid failedIds
                    in
                    { model
                        | selectedInboxTransactionIds = Set.diff model.selectedInboxTransactionIds acceptedIds
                        , inboxTransactions = List.filter (.id >> shouldBeLeft) model.inboxTransactions
                    }
                        |> noCmd
                        |> noCmd

                Err err ->
                    let
                        _ =
                            Debug.todo "err" err
                    in
                    model
                        |> noCmd
                        |> noCmd



-- VIEW --


view : Context msg -> Element msg
view ctx =
    let
        { model, session } =
            ctx
    in
    column []
        [ hasAnyNewTransaction session.user.id model.inboxTransactions
            |> either (viewInbox ctx) Element.none
        ]


viewInbox : Context msg -> Element msg
viewInbox ctx =
    let
        { model, session } =
            ctx
    in
    column [ spacing 15 ]
        [ row [ width fill, paddingEach { edges | right = 20 } ]
            [ text "Inbox"
            , el [ Font.size 16 ] <|
                text <|
                    (" ("
                        ++ (List.length model.inboxTransactions |> String.fromInt)
                        ++ ")"
                    )
            , styledButton [ alignRight, centerY ]
                { onPress =
                    someSelected model
                        |> either
                            (Just <| ctx.lift <| AcceptSelectedTransactions)
                            Nothing
                , label = text "Accept selected"
                }
            ]
        , table [ Font.size 14, spacing 8 ]
            { data = model.inboxTransactions
            , columns =
                [ { header =
                        Input.button
                            [ userSelectNone
                            , noShadow
                            , Font.color gray500 |> attrWhen (someSelected model && not (allSelected model))
                            , width shrink
                            ]
                            { onPress = Just <| (ctx.lift <| ToggleCheckAllInboxTransactions)
                            , label =
                                if someSelected model then
                                    viewIcon [] "check"

                                else
                                    viewIcon [] "check-empty"
                            }
                  , width = shrink
                  , view =
                        \t ->
                            Input.checkbox [ userSelectNone ]
                                { onChange = \selected -> ctx.lift <| ToggleInboxTransaction t.id
                                , icon = either "check" "check-empty" >> viewIcon []
                                , checked = Set.member t.id model.selectedInboxTransactionIds
                                , label = Input.labelRight [] <| text ""
                                }
                  }
                , { header = el [ Font.bold ] <| text "Tags"
                  , width = fill
                  , view = \t -> text (String.join ", " t.tags)
                  }
                , { header = el [ Font.bold ] <| text "Amount"
                  , width = px 100
                  , view = \t -> text (t.amount |> amountToMoney |> String.fromFloat)
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
                ]
            }
        ]


viewHeader : String -> Element msg
viewHeader str =
    Element.paragraph [] [ Element.text str ]
