module Page.Mailbox exposing (Context, Model, Msg(..), init, reinit, update, view)

import Cmd.Extra
import Data.CommonData exposing (CommonData)
import Data.Context exposing (ContextData, GlobalMsg(..), Logged)
import Data.Person exposing (personIdToName)
import Data.Session exposing (Session)
import Data.Transaction exposing (Transaction, TransactionEdit, TransactionId, amountDifferenceForMyAccount, amountToMoneyChangeLeftPad, amountToMoneyString, isTransactionInInboxForUser, isTransactionInOutboxForUser)
import Date exposing (Date)
import Element exposing (Element, above, alignBottom, alignRight, centerX, centerY, column, el, fill, fillPortion, height, inFront, minimum, padding, paddingEach, paddingXY, paragraph, px, rgba, row, shrink, spaceEvenly, spacing, table, text, width, wrappedRow)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Graphql.Http as GqlHttp
import I18n.I18n as I18n
import List.Extra
import Misc exposing (attrWhen, css, dayRelative, edges, either, noCmd, styledButton, viewIf, viewLoadingBar, viewModal)
import Misc.Colors as Colors
import Process
import RemoteData exposing (RemoteData)
import Request.Common exposing (sendMutationRequest, sendQueryRequest)
import Request.EditTransaction exposing (editTransaction)
import Request.Transactions exposing (AcceptTransactionsResult, TransactionList, acceptTransactions, getUserMailboxTransactions)
import Task
import Time
import Views.TransactionComposeForm as TransactionComposeForm exposing (Event(..), ViewState(..))


type alias Context msg =
    Logged (ContextData Model Msg msg)



-- MODEL --


type alias Model =
    { editingTransaction : Maybe EditingTransaction
    , allTransactions : Maybe (List TransactionView)
    , boxType : BoxType
    }


type BoxType
    = Inbox
    | Outbox


type alias TransactionView =
    { data : Transaction
    , animateToBeDeleted : Bool
    }


type alias EditingTransaction =
    { id : TransactionId
    , form : TransactionComposeForm.Model
    , viewState : ViewState
    }


init : Session -> ( Model, Cmd Msg )
init session =
    ( { allTransactions = Nothing
      , editingTransaction = Nothing
      , boxType = Inbox
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



-- UPDATE --


type Msg
    = SwitchBoxType
    | RefreshTransactions
    | RefreshTransactions_Response (RemoteData (GqlHttp.Error TransactionList) TransactionList)
    | RemoveMarkedTransactionsFromList
    | OpenTransactionEdit TransactionId
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
    in
    case topMsg of
        SwitchBoxType ->
            { model
                | boxType =
                    case model.boxType of
                        Inbox ->
                            Outbox

                        Outbox ->
                            Inbox
            }
                |> noCmd
                |> noCmd

        RefreshTransactions ->
            let
                cmd =
                    getUserMailboxTransactions
                        |> sendQueryRequest RefreshTransactions_Response
            in
            ( ( model, cmd ), Cmd.none )

        RefreshTransactions_Response (RemoteData.Success transactionList) ->
            let
                newModel =
                    { model
                        | allTransactions =
                            Just <|
                                List.filterMap
                                    (\t ->
                                        if isTransactionInInboxForUser session.id t || isTransactionInOutboxForUser session.id t then
                                            Just { data = t, animateToBeDeleted = False }

                                        else
                                            Nothing
                                    )
                                    transactionList.transactions
                    }
            in
            newModel
                |> noCmd
                |> noCmd

        RefreshTransactions_Response _ ->
            ( ( model, Cmd.none ), Cmd.none )

        RemoveMarkedTransactionsFromList ->
            let
                newTransactions =
                    case model.allTransactions of
                        Nothing ->
                            Nothing

                        Just list ->
                            Just (list |> List.filter (\t -> not t.animateToBeDeleted))

                newModel =
                    { model | allTransactions = newTransactions }

                cmdUpdateInboxSizeBadge =
                    case newModel.allTransactions of
                        Nothing ->
                            Cmd.none

                        Just list ->
                            Cmd.Extra.perform <| UpdateInboxSize <| List.length list
            in
            newModel
                |> noCmd
                |> Cmd.Extra.with cmdUpdateInboxSizeBadge

        OpenTransactionEdit transactionId ->
            let
                transaction : Maybe Transaction
                transaction =
                    model.allTransactions
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
            { model | editingTransaction = edit }
                |> noCmd
                |> noCmd

        CancelTransactionEdit ->
            { model | editingTransaction = Nothing }
                |> noCmd
                |> Cmd.Extra.with (Cmd.Extra.perform <| SetScrollbarsVisibility True)

        GotComposingFormMsg subMsg ->
            model.editingTransaction
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

                            newModel =
                                { model | editingTransaction = Just newEdit }

                            passEvents : List (Cmd Msg)
                            passEvents =
                                List.map
                                    (\evt ->
                                        case evt of
                                            OnSaveTransaction t ->
                                                SaveTransaction edit.id t |> Cmd.Extra.perform

                                            OnCloseClicked ->
                                                CancelTransactionEdit |> Cmd.Extra.perform
                                    )
                                    events

                            cmds =
                                Cmd.batch <| Cmd.map GotComposingFormMsg nextSubMsg :: passEvents
                        in
                        Just <| (newModel |> Cmd.Extra.with cmds |> Cmd.Extra.with globalMsg)
                    )
                |> Maybe.withDefault (model |> noCmd |> noCmd)

        SaveTransaction transactionId transactionEdit ->
            model.editingTransaction
                |> Maybe.andThen
                    (\edit ->
                        let
                            newEdit =
                                { edit | viewState = EditSaving }

                            newModel =
                                { model | editingTransaction = Just newEdit }

                            sendReqCmd =
                                transactionEdit
                                    |> editTransaction transactionId
                                    |> sendMutationRequest SaveTransaction_Response
                        in
                        Just <|
                            (newModel
                                |> Cmd.Extra.with sendReqCmd
                                |> Cmd.Extra.with (Cmd.Extra.perform <| SetScrollbarsVisibility True)
                            )
                    )
                |> Maybe.withDefault (model |> noCmd |> noCmd)

        SaveTransaction_Response result ->
            result
                |> RemoteData.map
                    (\editedTransaction ->
                        let
                            shouldDelete =
                                model.boxType == Inbox

                            newTransactions =
                                model.allTransactions
                                    |> Maybe.andThen
                                        (Just
                                            << List.Extra.updateIf
                                                (\t -> t.data.id == editedTransaction.id)
                                                (\t -> { t | data = editedTransaction, animateToBeDeleted = shouldDelete })
                                        )

                            cmdDeleteTransaction =
                                if shouldDelete then
                                    Process.sleep removeAnimationDuration
                                        |> Task.perform (always <| RemoveMarkedTransactionsFromList)

                                else
                                    Cmd.none

                            newModel =
                                { model | allTransactions = newTransactions, editingTransaction = Nothing }
                        in
                        newModel
                            |> Cmd.Extra.with cmdDeleteTransaction
                            |> noCmd
                    )
                |> RemoteData.withDefault (model |> noCmd |> noCmd)

        AcceptTransaction transactionId ->
            let
                cmdReq =
                    acceptTransactions [ transactionId ]
                        |> sendMutationRequest AcceptTransaction_Response
            in
            model |> Cmd.Extra.with cmdReq |> noCmd

        AcceptTransaction_Response result ->
            result
                |> RemoteData.map
                    (\{ acceptedIds, failedIds } ->
                        case ( acceptedIds, failedIds ) of
                            ( id :: [], [] ) ->
                                let
                                    newTransactions =
                                        case model.allTransactions of
                                            Nothing ->
                                                Nothing

                                            Just list ->
                                                Just <|
                                                    List.Extra.updateIf
                                                        (\t -> t.data.id == id)
                                                        (\t -> { t | animateToBeDeleted = True })
                                                        list

                                    newModel =
                                        { model | allTransactions = newTransactions }

                                    cmdDeleteTransaction =
                                        Process.sleep removeAnimationDuration
                                            |> Task.perform (always <| RemoveMarkedTransactionsFromList)
                                in
                                newModel |> Cmd.Extra.with cmdDeleteTransaction |> noCmd

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

        filterFunc =
            case model.boxType of
                Inbox ->
                    isTransactionInInboxForUser session.id

                Outbox ->
                    isTransactionInOutboxForUser session.id

        filteredTransactions =
            model.allTransactions |> Maybe.map (List.filter (.data >> filterFunc))

        viewSwitchButton boxType =
            styledButton [ width <| fillPortion 1 ]
                { onPress = Just <| ctx.lift <| SwitchBoxType
                , label =
                    case boxType of
                        Inbox ->
                            text "Outbox"

                        Outbox ->
                            text "Inbox"
                }
    in
    column []
        [ row [ centerX, padding 5, spacing 5 ]
            (case model.boxType of
                Inbox ->
                    [ text "Inbox |", viewSwitchButton model.boxType ]

                Outbox ->
                    [ viewSwitchButton model.boxType, text "| Outbox" ]
            )
        , case filteredTransactions of
            Just transactions ->
                case transactions of
                    [] ->
                        el [ padding 15 ] <| text "The box is empty."

                    theTransactions ->
                        viewBox ctx theTransactions

            Nothing ->
                viewLoadingBar
        ]


{-| List of all cards and UI on top
-}
viewBox : Context msg -> List TransactionView -> Element msg
viewBox ctx transactions =
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
        List.map (viewCard ctx) transactions


viewTransactionEdit : Context msg -> EditingTransaction -> Element msg
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


viewCard : Context msg -> TransactionView -> Element msg
viewCard ctx transaction =
    let
        diff =
            amountDifferenceForMyAccount ctx.session.id transaction.data

        diffAmountEl =
            diff
                |> amountToMoneyChangeLeftPad True 0
                |> text
                |> Element.el [ Font.color Colors.white ]

        isShared =
            transaction.data.amount /= abs diff

        totalAmountEl =
            row [ Font.color Colors.gray300 ]
                [ viewIf isShared <|
                    (text <| " / " ++ amountToMoneyString transaction.data.amount)
                , el [ Font.size 12, Element.alignBottom, paddingEach { edges | bottom = 1 } ] <|
                    text (" " ++ I18n.currency.suffixOrCode)
                ]

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
            ]
        , viewDetails ctx transaction.data
        ]


viewDetails : Context msg -> Transaction -> Element msg
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


viewButtons : Context msg -> Transaction -> Element msg
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
        , case ctx.model.boxType of
            Inbox ->
                styledButton [ Background.color Colors.teal400 ]
                    { onPress = Just <| ctx.lift <| AcceptTransaction transaction.id
                    , label = text "Accept"
                    }

            Outbox ->
                text ""
        ]
