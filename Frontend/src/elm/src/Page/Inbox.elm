module Page.Inbox exposing (Model, Msg, init, reinit, update, view)

import Array exposing (Array)
import Cmd.Extra
import Data.Context exposing (ContextData, GlobalMsg(..), Logged)
import Data.Person exposing (Person, PersonId, personIdToName)
import Data.Session exposing (Session)
import Data.Transaction exposing (Transaction, TransactionId, amountDifferenceForMyAccount, amountToMoney, amountToMoneyChangeLeftPad, amountToMoneyString, isTransactionInInboxForUser)
import Date
import Element exposing (Element, above, alignBottom, alignRight, centerX, centerY, column, el, fill, height, inFront, minimum, padding, paddingEach, paddingXY, paragraph, px, row, shrink, spacing, table, text, width, wrappedRow)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Graphql.Http as GqlHttp
import Json.Decode as Json
import List.Extra
import Misc exposing (attrWhen, css, dayRelative, edges, either, noCmd, noShadow, styledButton, userSelectNone, viewIcon, viewIf, viewLoadingBar)
import Misc.Colors as Colors
import Misc.DataExtra exposing (toggle)
import RemoteData exposing (RemoteData)
import Request.Common exposing (..)
import Request.Transactions exposing (AcceptTransactionsResult, TransactionList, acceptTransactions, getUserInboxTransactions)
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
    , initialCmds
    )


reinit : Model -> Session -> ( Model, Cmd Msg )
reinit model session =
    ( model, initialCmds )


initialCmds =
    Cmd.batch
        [ Cmd.Extra.perform RefreshTransactions
        , Task.attempt SetDate Time.now
        ]


type alias Context msg =
    Logged (ContextData Model Msg msg)


hasAnyNewTransaction : PersonId -> List Transaction -> Bool
hasAnyNewTransaction userId transactions =
    transactions |> List.any (isTransactionInInboxForUser userId)


allSelected inboxTransactions selectedInboxTransactionIds =
    List.length inboxTransactions == Set.size selectedInboxTransactionIds


someSelected model =
    not <| Set.isEmpty model.selectedInboxTransactionIds



-- UPDATE --


type Msg
    = RefreshTransactions
    | RefreshTransactions_Response (RemoteData (GqlHttp.Error TransactionList) TransactionList)
    | SetDate (Result String Posix)
      -- old view:
    | ToggleInboxTransaction TransactionId
    | ToggleCheckAllInboxTransactions
    | AcceptSelectedTransactions
    | AcceptSelectedTransactions_Response (RemoteData (GqlHttp.Error AcceptTransactionsResult) AcceptTransactionsResult)
      -- new view:
    | OpenTransactionEdit TransactionId
    | AcceptTransaction TransactionId
    | AcceptAllVisibleTransactions


update : Context msg -> Msg -> ( ( Model, Cmd Msg ), Cmd GlobalMsg )
update { model, session } msg =
    case msg of
        RefreshTransactions ->
            let
                cmd =
                    getUserInboxTransactions
                        |> sendQueryRequest RefreshTransactions_Response
            in
            ( ( model, cmd ), Cmd.none )

        RefreshTransactions_Response (RemoteData.Success transactionList) ->
            { model
                | inboxTransactions =
                    Just <|
                        List.filter (isTransactionInInboxForUser session.id) transactionList.transactions
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

                        transactionsLeft =
                            model.inboxTransactions
                                |> Maybe.map (List.filter (.id >> shouldBeLeft))
                                |> Maybe.withDefault []
                    in
                    ( { model
                        | selectedInboxTransactionIds = Set.diff model.selectedInboxTransactionIds acceptedIds
                        , inboxTransactions = Just transactionsLeft
                      }
                        |> noCmd
                    , Cmd.Extra.perform <| UpdateInboxSize <| List.length transactionsLeft
                    )

                _ ->
                    model
                        |> noCmd
                        |> noCmd

        OpenTransactionEdit transactionId ->
            model |> noCmd |> noCmd

        AcceptTransaction transactionId ->
            model |> noCmd |> noCmd

        AcceptAllVisibleTransactions ->
            model |> noCmd |> noCmd



-- VIEW --


view : Context msg -> Element msg
view ctx =
    let
        { model, session } =
            ctx
    in
    column [ Font.size 14 ]
        [ case model.inboxTransactions of
            Just inboxTransactions ->
                hasAnyNewTransaction session.id inboxTransactions
                    |> either (viewInbox ctx inboxTransactions) Element.none

            Nothing ->
                viewLoadingBar
        ]


{-| List of all cards and UI on top
-}
viewInbox : Context msg -> List Transaction -> Element msg
viewInbox ctx inboxTransactions =
    let
        { model, session } =
            ctx

        areAllSelected =
            allSelected inboxTransactions model.selectedInboxTransactionIds
    in
    wrappedRow
        [ css "justify-content" "center"
        , paddingXY 15 15
        , spacing 20
        ]
    <|
        List.map (viewCard ctx) inboxTransactions


viewCard : Context msg -> Transaction -> Element msg
viewCard ctx transaction =
    let
        diff =
            amountDifferenceForMyAccount ctx.session.id transaction

        diffAmountEl =
            diff
                |> amountToMoneyChangeLeftPad True 0
                |> text
                |> Element.el [ Font.color <| either Colors.green800 Colors.red500 (diff > 0) ]

        totalAmountEl =
            viewIf (transaction.amount /= abs diff) <|
                (Element.el [ Font.color Colors.gray400 ] <| text <| " / " ++ amountToMoneyString transaction.amount)

        transactionDate =
            Date.fromPosix Time.utc transaction.insertedAt

        datetimeEl =
            column
                [ alignRight
                , centerY
                , paddingEach { edges | right = 5 }
                , Font.size 11
                , spacing 2
                ]
                [ text <| Date.toIsoString transactionDate
                , Element.el [ Font.size 10, Font.color Colors.gray400 ] <| text <| "" ++ dayRelative ctx.todayDate transactionDate ++ ""
                ]
    in
    column
        [ width <| px 260
        , height <| minimum 140 fill
        , Border.widthXY 1 1
        , Border.rounded 2
        , Border.color Colors.teal200
        , inFront <| Element.el [ alignBottom, width fill ] <| viewButtons ctx transaction
        ]
        [ row
            [ Font.size 16
            , width fill
            , paddingEach { edges | left = 4, right = 10, top = 10, bottom = 8 }
            , Border.widthEach { edges | bottom = 1 }
            , Border.color Colors.teal400
            , inFront <| datetimeEl
            ]
            [ diffAmountEl
            , totalAmountEl
            , Element.el [ Font.color Colors.gray400 ] <| text " zł"
            ]
        , viewDetails ctx transaction
        ]


viewDetails : Context msg -> Transaction -> Element msg
viewDetails ctx t =
    let
        pidToName =
            personIdToName ctx.commonData.people
    in
    column
        [ paddingXY 10 7
        , spacing 7
        , width fill
        , inFront <|
            Element.el [ alignRight, paddingEach { edges | top = 5, right = 3 } ] <|
                viewTransactionTags True t
        ]
        [ row
            [ width fill ]
            [ text <| pidToName t.payorId
            , text <| " → "
            , text <| String.join ", " <| List.map pidToName t.beneficientIds
            ]
        , paragraph
            [ Font.size 13
            , Font.color Colors.gray500

            -- this padding is for `viewButtons` which is placed `inFront`
            , paddingEach
                { edges
                    | bottom = 25
                    , right = List.length t.tags > 1 |> either 80 0
                }
            ]
            [ text <| Maybe.withDefault "" t.description ]
        ]


viewTransactionTags : Bool -> Transaction -> Element msg
viewTransactionTags inColumn transaction =
    (inColumn |> either column wrappedRow)
        [ spacing 3 ]
        (List.map (\tag -> viewTag tag) transaction.tags)


viewTag : String -> Element msg
viewTag tag =
    Element.el
        [ Border.rounded 3
        , Background.color Colors.teal200
        , Font.color Colors.white
        , paddingXY 4 2
        , Font.size 12
        , alignRight
        ]
        (text tag)


viewButtons ctx transaction =
    row
        [ Background.color Colors.teal200
        , width fill
        , paddingXY 5 5
        ]
        [ text "Edit" ]



-- old view --


viewInbox_old : Context msg -> List Transaction -> Element msg
viewInbox_old ctx inboxTransactions =
    let
        { model, session } =
            ctx

        areAllSelected =
            allSelected inboxTransactions model.selectedInboxTransactionIds
    in
    column [ spacing 15 ]
        [ row [ width fill, paddingEach { edges | right = 20 } ]
            [ styledButton [ alignRight, centerY ]
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
                            , Font.color Colors.gray500 |> attrWhen (someSelected model && not areAllSelected)
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
                            amountDifferenceForMyAccount session.id t
                                |> amountToMoney
                                |> String.fromFloat
                                |> text
                  }
                ]
            }
        ]
