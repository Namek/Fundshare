module Page.Inbox exposing (Model, Msg, init, update, view)

import Array exposing (Array)
import Cmd.Extra
import Data.Context exposing (ContextData, GlobalMsg, Logged)
import Data.Person exposing (PersonId)
import Data.Session exposing (Session)
import Data.Transaction exposing (Transaction, TransactionId, amountDifferenceForMyAccount, amountToMoney, isTransactionUnseenForUser)
import Element exposing (Element, alignRight, centerY, column, el, fill, padding, paddingEach, paragraph, px, row, shrink, spacing, table, text, width)
import Element.Font as Font
import Element.Input as Input
import Graphql.Http as GqlHttp
import Json.Decode as Json
import List.Extra
import Misc exposing (attrWhen, edges, either, noCmd, noShadow, styledButton, toggle, userSelectNone, viewIcon)
import Misc.Colors exposing (gray500)
import RemoteData exposing (RemoteData)
import Request.Common exposing (..)
import Request.Transactions exposing (AcceptTransactionsResult, acceptTransactions, getUserTransactions)
import Set exposing (Set)
import Task
import Time exposing (Posix, now)



-- MODEL --


type alias Model =
    { inboxTransactions : Maybe (List Transaction)
    , selectedInboxTransactionIds : Set TransactionId
    }


init : Session -> ( Model, Cmd Msg )
init session =
    ( { inboxTransactions = Nothing
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


allSelected inboxTransactions selectedInboxTransactionIds =
    List.length inboxTransactions == Set.size selectedInboxTransactionIds


someSelected model =
    not <| Set.isEmpty model.selectedInboxTransactionIds



-- UPDATE --


type Msg
    = RefreshTransactions
    | RefreshTransactions_Response (RemoteData (GqlHttp.Error (List Transaction)) (List Transaction))
    | SetDate (Result String Posix)
    | ToggleInboxTransaction TransactionId
    | ToggleCheckAllInboxTransactions
    | AcceptSelectedTransactions
    | AcceptSelectedTransactions_Response (RemoteData (GqlHttp.Error AcceptTransactionsResult) AcceptTransactionsResult)


update : Context msg -> Msg -> ( ( Model, Cmd Msg ), Cmd GlobalMsg )
update { model, session } msg =
    case msg of
        RefreshTransactions ->
            let
                cmd =
                    getUserTransactions
                        |> sendQueryRequest RefreshTransactions_Response
            in
            ( ( model, cmd ), Cmd.none )

        RefreshTransactions_Response (RemoteData.Success transactions) ->
            { model
                | inboxTransactions = Just <| List.filter (isTransactionUnseenForUser session.user.id) transactions
            }
                |> noCmd
                |> noCmd

        RefreshTransactions_Response _ ->
            ( ( model, Cmd.none ), Cmd.none )

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
                        model.inboxTransactions
                            |> Maybe.map (List.map .id)
                            |> Maybe.withDefault []
                            |> Set.fromList
            }
                |> noCmd
                |> noCmd

        AcceptSelectedTransactions ->
            let
                cmd =
                    model.selectedInboxTransactionIds
                        |> Set.toList
                        |> acceptTransactions
                        |> sendMutationRequest AcceptSelectedTransactions_Response
            in
            ( model, cmd ) |> noCmd

        AcceptSelectedTransactions_Response response ->
            case response of
                RemoteData.Success data ->
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
                        , inboxTransactions =
                            model.inboxTransactions |> Maybe.map (List.filter (.id >> shouldBeLeft))
                    }
                        |> noCmd
                        |> noCmd

                _ ->
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
        [ case model.inboxTransactions of
            Just inboxTransactions ->
                hasAnyNewTransaction session.user.id inboxTransactions
                    |> either (viewInbox ctx inboxTransactions) Element.none

            Nothing ->
                text "Loading..."
        ]


viewInbox : Context msg -> List Transaction -> Element msg
viewInbox ctx inboxTransactions =
    let
        { model, session } =
            ctx

        areAllSelected =
            allSelected inboxTransactions model.selectedInboxTransactionIds
    in
    column [ spacing 15 ]
        [ row [ width fill, paddingEach { edges | right = 20 } ]
            [ text "Inbox"
            , el [ Font.size 16 ] <|
                text <|
                    (" ("
                        ++ (List.length inboxTransactions |> String.fromInt)
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
            { data = inboxTransactions
            , columns =
                [ { header =
                        Input.button
                            [ userSelectNone
                            , noShadow
                            , Font.color gray500 |> attrWhen (someSelected model && not areAllSelected)
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
                            amountDifferenceForMyAccount session.user.id t
                                |> amountToMoney
                                |> String.fromFloat
                                |> text
                  }
                ]
            }
        ]
