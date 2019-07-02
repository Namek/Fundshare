module Page.Inbox exposing (Context, Model, Msg(..), init, reinit, update, view)

import Cmd.Extra
import Data.CommonData exposing (CommonData)
import Data.Context exposing (ContextData, GlobalMsg(..), Logged)
import Data.Person exposing (personIdToName)
import Data.Session exposing (Session)
import Data.Transaction exposing (Transaction, TransactionEdit, TransactionId, amountDifferenceForMyAccount, amountToMoneyChangeLeftPad, amountToMoneyString, isTransactionInInboxForUser)
import Date exposing (Date)
import Element exposing (Element, above, alignBottom, alignRight, centerX, centerY, column, el, fill, height, inFront, minimum, padding, paddingEach, paddingXY, paragraph, px, rgba, row, shrink, spaceEvenly, spacing, table, text, width, wrappedRow)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Graphql.Http as GqlHttp
import List.Extra
import Misc exposing (attrWhen, css, dayRelative, edges, either, noCmd, styledButton, viewIf, viewLoadingBar, viewModal)
import Misc.Colors as Colors
import Process
import RemoteData exposing (RemoteData)
import Request.Common exposing (sendMutationRequest, sendQueryRequest)
import Request.EditTransaction exposing (editTransaction)
import Request.Transactions exposing (AcceptTransactionsResult, TransactionList, acceptTransactions, getUserInboxTransactions)
import Task
import Time
import Views.TransactionComposeForm as TransactionComposeForm exposing (Event(..), ViewState(..))


type alias Context msg =
    Logged (ContextData Model Msg msg)



-- MODEL --


type alias Model =
    { editingTransaction : Maybe EditingTransaction
    , inboxTransactions : Maybe (List TransactionView)
    }


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
    ( { inboxTransactions = Nothing
      , editingTransaction = Nothing
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
    = RefreshTransactions
    | RefreshTransactions_Response (RemoteData (GqlHttp.Error TransactionList) TransactionList)
    | RemoveTransactionFromList TransactionId
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
        RefreshTransactions ->
            let
                cmd =
                    getUserInboxTransactions
                        |> sendQueryRequest RefreshTransactions_Response
            in
            ( ( model, cmd ), Cmd.none )

        RefreshTransactions_Response (RemoteData.Success transactionList) ->
            let
                newModel =
                    { model
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
            newModel
                |> noCmd
                |> noCmd

        RefreshTransactions_Response _ ->
            ( ( model, Cmd.none ), Cmd.none )

        RemoveTransactionFromList transactionId ->
            let
                newInboxTransactions =
                    case model.inboxTransactions of
                        Nothing ->
                            Nothing

                        Just list ->
                            Just (list |> List.filter (\t -> not t.animateToBeDeleted))

                newModel =
                    { model | inboxTransactions = newInboxTransactions }

                cmdUpdateInboxSizeBadge =
                    case newModel.inboxTransactions of
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
                    model.inboxTransactions
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
                |> Cmd.Extra.with (Cmd.Extra.perform <| SetScrollbarsVisibility False)

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
                        Just <| (newModel |> Cmd.Extra.with sendReqCmd |> Cmd.Extra.pure)
                    )
                |> Maybe.withDefault (model |> noCmd |> noCmd)

        SaveTransaction_Response result ->
            result
                |> RemoteData.map
                    (\editedTransaction ->
                        let
                            newTransactions =
                                model.inboxTransactions
                                    |> Maybe.andThen
                                        (Just
                                            << List.Extra.updateIf (\t -> t.data.id == editedTransaction.id)
                                                (\t -> { data = editedTransaction, animateToBeDeleted = True })
                                        )

                            cmdDeleteTransaction =
                                Process.sleep removeAnimationDuration
                                    |> Task.perform (always <| RemoveTransactionFromList editedTransaction.id)

                            newModel =
                                { model | inboxTransactions = newTransactions, editingTransaction = Nothing }
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
                                    newInboxTransactions =
                                        case model.inboxTransactions of
                                            Nothing ->
                                                Nothing

                                            Just list ->
                                                Just <|
                                                    List.Extra.updateIf
                                                        (\t -> t.data.id == id)
                                                        (\t -> { t | animateToBeDeleted = True })
                                                        list

                                    newModel =
                                        { model | inboxTransactions = newInboxTransactions }

                                    cmdDeleteTransaction =
                                        Process.sleep removeAnimationDuration
                                            |> Task.perform (always <| RemoveTransactionFromList id)
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


type alias ViewCtx parentMsg =
    { lift : Msg -> parentMsg
    , model : Model
    , session : Session
    , todayDate : Date
    , commonData : CommonData
    }


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
            viewInbox_cards
                { lift = ctx.lift
                , model = ctx.model
                , session = ctx.session
                , todayDate = ctx.todayDate
                , commonData = ctx.commonData
                }
                transactions
    in
    case model.inboxTransactions of
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
viewInbox_cards : ViewCtx msg -> List TransactionView -> Element msg
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


viewTransactionEdit : ViewCtx msg -> EditingTransaction -> Element msg
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


viewCard : ViewCtx msg -> TransactionView -> Element msg
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


viewDetails : ViewCtx msg -> Transaction -> Element msg
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


viewButtons : ViewCtx msg -> Transaction -> Element msg
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
