module Page.Inbox exposing (Model, Msg, init, reinit, update, view)

import Array exposing (Array)
import Cmd.Extra
import Data.CommonData exposing (CommonData)
import Data.Context exposing (ContextData, GlobalMsg(..), Logged)
import Data.Person exposing (Person, PersonId, personIdToName)
import Data.Session exposing (Session)
import Data.Transaction exposing (Transaction, TransactionId, amountDifferenceForMyAccount, amountToMoney, amountToMoneyChangeLeftPad, amountToMoneyString, isTransactionInInboxForUser)
import Date exposing (Date)
import Element exposing (Element, above, alignBottom, alignRight, centerX, centerY, column, el, fill, height, inFront, minimum, padding, paddingEach, paddingXY, paragraph, px, rgba, row, shrink, spaceEvenly, spacing, table, text, width, wrappedRow)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input exposing (button)
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


type alias Context msg =
    Logged (ContextData Model Msg msg)



-- MODEL --


type alias Model =
    { viewType : CardsViewType
    , table : ModelTable
    , cards : ModelCards
    , common : ModelCommon
    }


type CardsViewType
    = Table
    | Cards


type alias ModelTable =
    { selectedInboxTransactionIds : Set TransactionId
    }


type alias ModelCards =
    { editingTransaction : Maybe { id : TransactionId } }


type alias ModelCommon =
    { inboxTransactions : Maybe (List Transaction) }


type alias ViewCtx parentMsg localMsg localModel =
    { lift : localMsg -> parentMsg
    , model : localModel
    , modelCommon : ModelCommon
    , session : Session
    , commonData : CommonData
    , todayDate : Date
    }


type alias ViewNew msg =
    ViewCtx msg MsgCards ModelCards


type alias ViewOld msg =
    ViewCtx msg MsgTable ModelTable


init : Session -> ( Model, Cmd Msg )
init session =
    ( { viewType = Cards
      , common = { inboxTransactions = Nothing }
      , table =
            { selectedInboxTransactionIds = Set.empty
            }
      , cards = { editingTransaction = Nothing }
      }
    , initialCmds
    )


reinit : Model -> Session -> ( Model, Cmd Msg )
reinit model session =
    ( model, initialCmds )


initialCmds : Cmd Msg
initialCmds =
    Cmd.batch
        [ Cmd.Extra.perform RefreshTransactions
        , Task.attempt SetDate Time.now
        ]


hasAnyNewTransaction : PersonId -> List Transaction -> Bool
hasAnyNewTransaction userId transactions =
    transactions |> List.any (isTransactionInInboxForUser userId)


allSelected inboxTransactions selectedInboxTransactionIds =
    List.length inboxTransactions == Set.size selectedInboxTransactionIds


someSelected model =
    not <| Set.isEmpty model.selectedInboxTransactionIds



-- UPDATE --


type Msg
    = MsgTable MsgTable
    | MsgCards MsgCards
    | ToggleViewMode
      -- common messages
    | RefreshTransactions
    | RefreshTransactions_Response (RemoteData (GqlHttp.Error TransactionList) TransactionList)
    | SetDate (Result String Posix)


type MsgTable
    = ToggleInboxTransaction TransactionId
    | ToggleCheckAllInboxTransactions
    | AcceptSelectedTransactions
    | AcceptSelectedTransactions_Response (RemoteData (GqlHttp.Error AcceptTransactionsResult) AcceptTransactionsResult)


type MsgCards
    = OpenTransactionEdit TransactionId
    | CancelTransactionEdit
    | AcceptTransactionEdit
    | AcceptTransaction TransactionId
    | AcceptAllVisibleTransactions


update : Context msg -> Msg -> ( ( Model, Cmd Msg ), Cmd GlobalMsg )
update ctx topMsg =
    let
        { model, session } =
            ctx

        modelCommon =
            model.common
    in
    case topMsg of
        RefreshTransactions ->
            let
                cmd =
                    getUserInboxTransactions
                        |> sendQueryRequest RefreshTransactions_Response
            in
            ( ( model, cmd ), Cmd.none )

        RefreshTransactions_Response (RemoteData.Success transactionList) ->
            let
                newModelCommon =
                    { modelCommon
                        | inboxTransactions =
                            Just <|
                                List.filter (isTransactionInInboxForUser session.id) transactionList.transactions
                    }
            in
            { model | common = newModelCommon }
                |> noCmd
                |> noCmd

        RefreshTransactions_Response _ ->
            ( ( model, Cmd.none ), Cmd.none )

        SetDate dateStringResult ->
            ( ( model, Cmd.none ), Cmd.none )

        MsgTable msgTable ->
            let
                ( ( newModel, cmds ), globalCmds ) =
                    updateTable ctx msgTable
            in
            ( ( newModel, cmds ), globalCmds )

        MsgCards msgCards ->
            let
                ( ( newModel, cmds ), globalCmds ) =
                    updateCards ctx msgCards
            in
            ( ( newModel, cmds ), globalCmds )

        ToggleViewMode ->
            let
                newViewType =
                    case ctx.model.viewType of
                        Cards ->
                            Table

                        Table ->
                            Cards
            in
            ( ( { model | viewType = newViewType }, Cmd.none ), Cmd.none )


updateTable : Context msg -> MsgTable -> ( ( Model, Cmd Msg ), Cmd GlobalMsg )
updateTable { model, session } msg =
    let
        lift =
            MsgTable

        modelTable =
            model.table

        modelCommon =
            model.common
    in
    case msg of
        ToggleInboxTransaction tid ->
            { model
                | table =
                    { modelTable | selectedInboxTransactionIds = toggle tid modelTable.selectedInboxTransactionIds }
            }
                |> noCmd
                |> noCmd

        ToggleCheckAllInboxTransactions ->
            { model
                | table =
                    { modelTable
                        | selectedInboxTransactionIds =
                            if someSelected modelTable then
                                Set.empty

                            else
                                modelCommon.inboxTransactions
                                    |> Maybe.map (List.map .id)
                                    |> Maybe.withDefault []
                                    |> Set.fromList
                    }
            }
                |> noCmd
                |> noCmd

        AcceptSelectedTransactions ->
            let
                cmd =
                    modelTable.selectedInboxTransactionIds
                        |> Set.toList
                        |> acceptTransactions
                        |> sendMutationRequest AcceptSelectedTransactions_Response
                        |> Cmd.map lift
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
                            modelCommon.inboxTransactions
                                |> Maybe.map (List.filter (.id >> shouldBeLeft))
                                |> Maybe.withDefault []

                        newModelTable =
                            { modelTable
                                | selectedInboxTransactionIds = Set.diff modelTable.selectedInboxTransactionIds acceptedIds
                            }

                        newModelCommon =
                            { modelCommon | inboxTransactions = Just transactionsLeft }

                        newModel =
                            { model | table = newModelTable, common = newModelCommon }
                    in
                    ( newModel
                        |> noCmd
                    , Cmd.Extra.perform <| UpdateInboxSize <| List.length transactionsLeft
                    )

                _ ->
                    model
                        |> noCmd
                        |> noCmd


updateCards : Context msg -> MsgCards -> ( ( Model, Cmd Msg ), Cmd GlobalMsg )
updateCards { model, session } msg =
    let
        lift =
            MsgCards

        modelCards =
            model.cards

        modelCommon =
            model.common
    in
    case msg of
        OpenTransactionEdit transactionId ->
            { model | cards = { modelCards | editingTransaction = Just { id = transactionId } } }
                |> noCmd
                |> noCmd

        CancelTransactionEdit ->
            { model | cards = { modelCards | editingTransaction = Nothing } } |> noCmd |> noCmd

        AcceptTransactionEdit ->
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

        viewInbox : List Transaction -> Element msg
        viewInbox =
            case model.viewType of
                Table ->
                    { lift = ctx.lift << MsgTable
                    , model = ctx.model.table
                    , modelCommon = ctx.model.common
                    , session = ctx.session
                    , commonData = ctx.commonData
                    , todayDate = ctx.todayDate
                    }
                        |> viewInbox_old

                Cards ->
                    { lift = ctx.lift << MsgCards
                    , model = ctx.model.cards
                    , modelCommon = ctx.model.common
                    , session = ctx.session
                    , commonData = ctx.commonData
                    , todayDate = ctx.todayDate
                    }
                        |> viewInbox_cards
    in
    column [ Font.size 14 ]
        [ Misc.styledButton [ centerX ]
            { onPress = Just <| ctx.lift <| ToggleViewMode
            , label = text "Toggle view mode"
            }
        , case model.common.inboxTransactions of
            Just inboxTransactions ->
                hasAnyNewTransaction session.id inboxTransactions
                    |> either (viewInbox inboxTransactions) Element.none

            Nothing ->
                viewLoadingBar
        ]


{-| List of all cards and UI on top
-}
viewInbox_cards : ViewNew msg -> List Transaction -> Element msg
viewInbox_cards ctx inboxTransactions =
    let
        { model, session } =
            ctx

        --        areAllSelected =
        --            allSelected inboxTransactions model.selectedInboxTransactionIds
    in
    case model.editingTransaction of
        Just edit ->
            row [] []

        Nothing ->
            wrappedRow
                [ css "justify-content" "center"
                , paddingXY 15 15
                , spacing 20
                ]
            <|
                List.map (viewCard ctx) inboxTransactions


viewTransactionEdit : ViewNew msg -> Element msg
viewTransactionEdit ctx =
    column [] [ text "transaction edit" ]


viewCard : ViewNew msg -> Transaction -> Element msg
viewCard ctx transaction =
    let
        diff =
            amountDifferenceForMyAccount ctx.session.id transaction

        diffAmountEl =
            diff
                |> amountToMoneyChangeLeftPad True 0
                |> text
                |> Element.el [ Font.color Colors.white ]

        totalAmountEl =
            viewIf (transaction.amount /= abs diff) <|
                (Element.el [ Font.color Colors.gray300 ] <| text <| " / " ++ amountToMoneyString transaction.amount)

        transactionDate =
            Date.fromPosix Time.utc transaction.insertedAt

        datetimeEl =
            column
                [ alignRight
                , centerY
                , paddingEach { edges | right = 10 }
                , Font.size 11
                , spacing 2
                ]
                [ Element.el [ Font.color Colors.gray100, centerX ] <| text <| Date.toIsoString transactionDate
                , Element.el [ Font.size 9, Font.color Colors.gray300, centerX ] <| text <| "" ++ dayRelative ctx.todayDate transactionDate ++ ""
                ]
    in
    column
        [ width <| px 260
        , height <| minimum 140 fill
        , Border.rounded 2
        , Background.color Colors.teal700
        , Border.shadow { offset = ( 3, 3 ), size = 1, blur = 20, color = rgba 0 0 0 0.2 }
        , inFront <|
            Element.el
                [ alignBottom
                , width fill
                ]
            <|
                viewButtons ctx transaction
        ]
        [ row
            [ Font.size 16
            , width fill
            , paddingEach { edges | left = 10, right = 10, top = 10, bottom = 8 }
            , Border.widthEach { edges | bottom = 1 }
            , Border.color Colors.teal400
            , inFront <| datetimeEl
            ]
            [ diffAmountEl
            , totalAmountEl
            , Element.el [ Font.color Colors.gray300 ] <| text " zł"
            ]
        , viewDetails ctx transaction
        ]


viewDetails : ViewNew msg -> Transaction -> Element msg
viewDetails ctx t =
    let
        pidToName =
            personIdToName ctx.commonData.people
    in
    paragraph
        [ paddingEach { edges | left = 10, top = 7, right = 10 }
        , spacing 7
        , width fill
        , Font.color Colors.gray200
        ]
        [ Element.el [ alignRight ] <|
            viewTransactionTags True t
        , row
            [ width fill ]
            [ text <| pidToName t.payorId
            , text <| " → "
            , text <| String.join ", " <| List.map pidToName t.beneficientIds
            ]
        , paragraph
            [ Font.size 13
            , Font.color Colors.gray300
            , paddingEach
                { edges
                  -- bottom padding is for `viewButtons` which is placed `inFront`
                    | bottom = 25
                    , top = 5
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


viewButtons : ViewNew msg -> Transaction -> Element msg
viewButtons ctx transaction =
    row
        [ width fill
        , paddingEach { edges | left = 9, right = 10, bottom = 10 }
        , spaceEvenly
        ]
        [ styledButton [ Background.color Colors.teal300, Font.color Colors.gray300 ]
            { onPress = Just <| ctx.lift <| OpenTransactionEdit transaction.id
            , label = text "Edit"
            }
        , styledButton [ Background.color Colors.teal400 ]
            { onPress = Just <| ctx.lift <| AcceptTransaction transaction.id
            , label = text "Accept"
            }
        ]



-- old view --


viewInbox_old : ViewOld msg -> List Transaction -> Element msg
viewInbox_old ctx inboxTransactions =
    let
        { session, model } =
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
                            (Just <| ctx.lift AcceptSelectedTransactions)
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
                            { onPress = Just <| ctx.lift ToggleCheckAllInboxTransactions
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
