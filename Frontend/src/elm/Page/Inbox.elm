module Page.Inbox exposing (Context, Model, Msg(..), MsgCards(..), init, reinit, update, view)

import Cmd.Extra
import Data.CommonData exposing (CommonData)
import Data.Context exposing (ContextData, GlobalMsg(..), Logged)
import Data.Person exposing (Person, PersonId, personIdToName)
import Data.Session exposing (Session)
import Data.Transaction exposing (Transaction, TransactionEdit, TransactionId, amountDifferenceForMyAccount, amountToMoney, amountToMoneyChangeLeftPad, amountToMoneyString, isTransactionInInboxForUser)
import Date exposing (Date)
import Element exposing (Element, above, alignBottom, alignRight, centerX, centerY, column, el, fill, height, inFront, minimum, padding, paddingEach, paddingXY, paragraph, px, rgba, row, shrink, spaceEvenly, spacing, table, text, width, wrappedRow)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Graphql.Http as GqlHttp
import List.Extra
import Misc exposing (attrWhen, css, dayRelative, edges, either, noCmd, noShadow, styledButton, userSelectNone, viewIcon, viewIf, viewLoadingBar, viewModal)
import Misc.Colors as Colors
import Misc.DataExtra exposing (toggle)
import Process
import RemoteData exposing (RemoteData)
import Request.Common exposing (..)
import Request.EditTransaction exposing (editTransaction)
import Request.Transactions exposing (AcceptTransactionsResult, TransactionList, acceptTransactions, getUserInboxTransactions)
import Set exposing (Set)
import Task
import Time exposing (Posix)
import Views.TransactionComposeForm as TransactionComposeForm exposing (Event(..), ViewState(..))


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
    { editingTransaction : Maybe EditingTransaction }


type alias ModelCommon =
    { inboxTransactions : Maybe (List TransactionView) }


type alias TransactionView =
    { data : Transaction
    , animateToBeDeleted : Bool
    }


type alias EditingTransaction =
    { id : TransactionId
    , form : TransactionComposeForm.Model
    , viewState : ViewState
    }


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
        [ Cmd.Extra.perform RefreshTransactions ]


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
    | RemoveTransactionFromList TransactionId


type MsgTable
    = ToggleInboxTransaction TransactionId
    | ToggleCheckAllInboxTransactions
    | AcceptSelectedTransactions
    | AcceptSelectedTransactions_Response (RemoteData (GqlHttp.Error AcceptTransactionsResult) AcceptTransactionsResult)


type MsgCards
    = OpenTransactionEdit TransactionId
    | GotComposingFormMsg TransactionComposeForm.Msg
    | CancelTransactionEdit
    | SaveTransaction TransactionId TransactionEdit
    | SaveTransaction_Response (RemoteData (GqlHttp.Error Transaction) Transaction)
    | AcceptTransaction TransactionId
    | AcceptTransaction_Response (RemoteData (GqlHttp.Error AcceptTransactionsResult) AcceptTransactionsResult)
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
                                List.filterMap
                                    (\t ->
                                        if isTransactionInInboxForUser session.id t then
                                            Just { data = t, animateToBeDeleted = False }

                                        else
                                            Nothing
                                    )
                                    transactionList.transactions
                    }
            in
            { model | common = newModelCommon }
                |> noCmd
                |> noCmd

        RefreshTransactions_Response _ ->
            ( ( model, Cmd.none ), Cmd.none )

        RemoveTransactionFromList transactionId ->
            let
                newInboxTransactions =
                    case modelCommon.inboxTransactions of
                        Nothing ->
                            Nothing

                        Just list ->
                            Just (list |> List.filter (\t -> not t.animateToBeDeleted))

                newModelCommon =
                    { modelCommon | inboxTransactions = newInboxTransactions }

                cmdUpdateInboxSizeBadge =
                    case newModelCommon.inboxTransactions of
                        Nothing ->
                            Cmd.none

                        Just list ->
                            Cmd.Extra.perform <| UpdateInboxSize <| List.length list
            in
            { model | common = newModelCommon }
                |> noCmd
                |> Cmd.Extra.with cmdUpdateInboxSizeBadge

        MsgTable msgTable ->
            let
                ( ( newModel, cmds ), globalCmds ) =
                    updateTable ctx msgTable
            in
            ( ( newModel, cmds ), globalCmds )

        MsgCards msgCards ->
            updateCards ctx msgCards

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
                                    |> Maybe.map (List.map (.data >> .id))
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
                                |> Maybe.map (List.filter (.data >> .id >> shouldBeLeft))
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
updateCards ctx msg =
    let
        { model } =
            ctx

        lift =
            MsgCards

        modelCards =
            model.cards

        modelCommon =
            model.common
    in
    case msg of
        OpenTransactionEdit transactionId ->
            let
                transaction : Maybe Transaction
                transaction =
                    model.common.inboxTransactions
                        |> Maybe.andThen (\ts -> List.Extra.find (\t -> t.data.id == transactionId) ts)
                        |> Maybe.map .data

                edit : Maybe EditingTransaction
                edit =
                    transaction
                        |> Maybe.map
                            (\t ->
                                let
                                    te : TransactionEdit
                                    te =
                                        { description = t.description
                                        , amount = t.amount
                                        , payorId = t.payorId
                                        , tags = t.tags
                                        , beneficientIds = t.beneficientIds
                                        }

                                    dateStr =
                                        t.insertedAt |> Date.fromPosix Time.utc |> Date.toIsoString

                                    et : EditingTransaction
                                    et =
                                        { id = transactionId
                                        , form = TransactionComposeForm.init ("Edit transaction: " ++ dateStr) True te
                                        , viewState = EditComposing
                                        }
                                in
                                et
                            )
            in
            { model | cards = { modelCards | editingTransaction = edit } }
                |> noCmd
                |> Cmd.Extra.with (Cmd.Extra.perform <| SetScrollbarsVisibility False)

        CancelTransactionEdit ->
            { model | cards = { modelCards | editingTransaction = Nothing } }
                |> noCmd
                |> Cmd.Extra.with (Cmd.Extra.perform <| SetScrollbarsVisibility True)

        GotComposingFormMsg subMsg ->
            modelCards.editingTransaction
                |> Maybe.andThen
                    (\edit ->
                        let
                            subCtx =
                                { model = edit.form
                                , lift = GotComposingFormMsg
                                , todayDate = ctx.todayDate
                                , commonData = ctx.commonData
                                , session = ctx.session
                                }

                            ( ( newModelForm, nextSubMsg ), globalMsg, events ) =
                                TransactionComposeForm.update subCtx subMsg

                            newEdit =
                                { edit | form = newModelForm }

                            newModelCards =
                                { modelCards | editingTransaction = Just newEdit }

                            passEvents : List (Cmd Msg)
                            passEvents =
                                List.map
                                    (\evt ->
                                        case evt of
                                            OnSaveTransaction t ->
                                                (SaveTransaction edit.id t |> lift) |> Cmd.Extra.perform

                                            OnCloseClicked ->
                                                CancelTransactionEdit |> lift |> Cmd.Extra.perform
                                    )
                                    events

                            cmds =
                                Cmd.batch <| Cmd.map (lift << GotComposingFormMsg) nextSubMsg :: passEvents
                        in
                        Just <| ({ model | cards = newModelCards } |> Cmd.Extra.with cmds |> Cmd.Extra.with globalMsg)
                    )
                |> Maybe.withDefault (model |> noCmd |> noCmd)

        SaveTransaction transactionId transactionEdit ->
            modelCards.editingTransaction
                |> Maybe.andThen
                    (\edit ->
                        let
                            newEdit =
                                { edit | viewState = EditSaving }

                            newModelCards =
                                { modelCards | editingTransaction = Just newEdit }

                            sendReqCmd =
                                transactionEdit
                                    |> editTransaction transactionId
                                    |> sendMutationRequest (lift << SaveTransaction_Response)
                        in
                        Just <| ({ model | cards = newModelCards } |> Cmd.Extra.with sendReqCmd |> Cmd.Extra.pure)
                    )
                |> Maybe.withDefault (model |> noCmd |> noCmd)

        SaveTransaction_Response result ->
            result
                |> RemoteData.map
                    (\editedTransaction ->
                        let
                            newTransactions =
                                modelCommon.inboxTransactions
                                    |> Maybe.andThen
                                        (Just
                                            << List.Extra.updateIf (\t -> t.data.id == editedTransaction.id)
                                                (\t -> { data = editedTransaction, animateToBeDeleted = True })
                                        )

                            cmdDeleteTransaction =
                                Process.sleep removeAnimationDuration
                                    |> Task.perform (always <| RemoveTransactionFromList editedTransaction.id)

                            newModelCommon =
                                { modelCommon | inboxTransactions = newTransactions }

                            newModelCards =
                                { modelCards | editingTransaction = Nothing }
                        in
                        { model | common = newModelCommon, cards = newModelCards }
                            |> Cmd.Extra.with cmdDeleteTransaction
                            |> noCmd
                    )
                |> RemoteData.withDefault (model |> noCmd |> noCmd)

        AcceptTransaction transactionId ->
            let
                cmdReq =
                    acceptTransactions [ transactionId ]
                        |> sendMutationRequest (lift << AcceptTransaction_Response)
            in
            model |> Cmd.Extra.with cmdReq |> noCmd

        AcceptTransaction_Response result ->
            result
                |> RemoteData.map
                    (\{ acceptedIds, failedIds } ->
                        case ( acceptedIds, failedIds ) of
                            ( id :: [], [] ) ->
                                let
                                    newInboxTransactions =
                                        case modelCommon.inboxTransactions of
                                            Nothing ->
                                                Nothing

                                            Just list ->
                                                Just <|
                                                    List.Extra.updateIf
                                                        (\t -> t.data.id == id)
                                                        (\t -> { t | animateToBeDeleted = True })
                                                        list

                                    newModelCommon =
                                        { modelCommon | inboxTransactions = newInboxTransactions }

                                    cmdDeleteTransaction =
                                        Process.sleep removeAnimationDuration
                                            |> Task.perform (always <| RemoveTransactionFromList id)
                                in
                                { model | common = newModelCommon } |> Cmd.Extra.with cmdDeleteTransaction |> noCmd

                            anything ->
                                let
                                    -- TODO notify about error?
                                    omg =
                                        Debug.log "transaction acceptance not happening" anything
                                in
                                model |> noCmd |> noCmd
                    )
                |> RemoteData.withDefault (model |> noCmd |> noCmd)

        AcceptAllVisibleTransactions ->
            model |> noCmd |> noCmd



-- VIEW --


{-| milliseconds
-}
removeAnimationDuration =
    450


view : Context msg -> Element msg
view ctx =
    let
        { model, session } =
            ctx

        viewInbox : List TransactionView -> Element msg
        viewInbox transactions =
            column []
                [ Misc.styledButton [ centerX ]
                    { onPress = Just <| ctx.lift <| ToggleViewMode
                    , label = text "Toggle view mode"
                    }
                , case model.viewType of
                    Table ->
                        viewInbox_old
                            { lift = ctx.lift << MsgTable
                            , model = ctx.model.table
                            , modelCommon = ctx.model.common
                            , session = ctx.session
                            , commonData = ctx.commonData
                            , todayDate = ctx.todayDate
                            }
                            (transactions |> List.map .data)

                    Cards ->
                        viewInbox_cards
                            { lift = ctx.lift << MsgCards
                            , model = ctx.model.cards
                            , modelCommon = ctx.model.common
                            , session = ctx.session
                            , commonData = ctx.commonData
                            , todayDate = ctx.todayDate
                            }
                            transactions
                ]
    in
    case model.common.inboxTransactions of
        Just inboxTransactions ->
            case inboxTransactions of
                [] ->
                    el [ padding 15 ] <| text "Inbox is empty."

                filteredTransactions ->
                    viewInbox filteredTransactions

        Nothing ->
            viewLoadingBar


{-| List of all cards and UI on top
-}
viewInbox_cards : ViewNew msg -> List TransactionView -> Element msg
viewInbox_cards ctx inboxTransactions =
    let
        { model } =
            ctx
    in
    wrappedRow
        [ css "justify-content" "center"
        , paddingXY 15 15
        , spacing 20
        , above <|
            case model.editingTransaction of
                Just edit ->
                    viewTransactionEdit ctx edit

                Nothing ->
                    Element.none
        ]
    <|
        List.map (viewCard ctx) inboxTransactions


viewTransactionEdit : ViewNew msg -> EditingTransaction -> Element msg
viewTransactionEdit ctx edit =
    let
        formCtx =
            { model = edit.form
            , commonData = ctx.commonData
            , todayDate = ctx.todayDate
            , lift = ctx.lift << GotComposingFormMsg
            , session = ctx.session
            }
    in
    viewModal [ TransactionComposeForm.viewForm formCtx edit.viewState ]


viewCard : ViewNew msg -> TransactionView -> Element msg
viewCard ctx transaction =
    let
        diff =
            amountDifferenceForMyAccount ctx.session.id transaction.data

        diffAmountEl =
            diff
                |> amountToMoneyChangeLeftPad True 0
                |> text
                |> Element.el [ Font.color Colors.white ]

        totalAmountEl =
            viewIf (transaction.data.amount /= abs diff) <|
                (Element.el [ Font.color Colors.gray300 ] <| text <| " / " ++ amountToMoneyString transaction.data.amount)

        transactionDate =
            Date.fromPosix Time.utc transaction.data.insertedAt

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
        , height <| minimum 160 fill
        , Border.rounded 2
        , Background.color Colors.teal700
        , Border.shadow { offset = ( 3, 3 ), size = 1, blur = 20, color = rgba 0 0 0 0.2 }
        , attrWhen (not transaction.animateToBeDeleted) <|
            inFront <|
                Element.el
                    [ alignBottom
                    , width fill
                    ]
                <|
                    viewButtons ctx transaction.data
        , attrWhen transaction.animateToBeDeleted <|
            css "transition" ("opacity " ++ String.fromInt removeAnimationDuration ++ "ms ease-in, transform 0.2s cubic-bezier(0.88, -0.24, 1, 0.77)")
        , attrWhen transaction.animateToBeDeleted <| css "opacity" "0"
        , attrWhen transaction.animateToBeDeleted <| css "transform" "scale(0)"
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
        , viewDetails ctx transaction.data
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
            [ width fill, Font.size 14 ]
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
