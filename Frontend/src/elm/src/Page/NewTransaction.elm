module Page.NewTransaction exposing (Model, Msg, init, update, view)

import Browser.Dom as Dom
import Cmd.Extra
import Data.Context exposing (..)
import Data.Person exposing (Person, PersonId)
import Data.Session exposing (Session)
import Data.Transaction exposing (TransactionEdit, TransactionId)
import Element exposing (Element, alignLeft, alignRight, below, centerX, centerY, column, el, explain, fill, fillPortion, focused, height, inFront, mouseDown, moveDown, moveLeft, moveUp, padding, paddingXY, paragraph, px, rgb255, rgba255, row, shrink, spaceEvenly, spacing, text, width, wrappedRow)
import Element.Background as Bg
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input exposing (labelRight)
import Graphql.Http
import Html.Attributes as Attr
import Html.Events exposing (keyCode)
import List
import List.Extra
import Maybe.Extra exposing (isJust, isNothing)
import Misc exposing (attr, attrWhen, either, match, moneyRegex, noCmd, noShadow, userSelectNone, viewIcon, viewIconButton, viewIf)
import Misc.Colors as Colors exposing (blue500, rgbHex, teal100, teal500, teal700, white)
import Misc.SuccessAnimation exposing (successAnim)
import RemoteData exposing (RemoteData)
import Request.AddTransaction exposing (..)
import Request.Common exposing (..)
import Request.People exposing (..)
import Route
import Set exposing (Set)
import Views.TransactionComposeForm as TransactionComposeForm exposing (Command(..), Event(..), ViewState(..))


type alias Context msg =
    Logged (ContextData Model Msg msg)



-- MODEL


type alias Model =
    { form : TransactionComposeForm.Model
    , saveState : SaveState
    }


type SaveState
    = Composing
    | Saving
    | SaveError
    | Saved TransactionId



--type alias SaveResult =
--    { paymentId : TransactionId }


init : Session -> ( Model, Cmd Msg )
init session =
    let
        model =
            initModel session
    in
    ( model, Cmd.Extra.perform (GotComposingFormMsg <| TransactionComposeForm.command DoFocusAmountInput) )


initModel : Session -> Model
initModel session =
    { form =
        TransactionComposeForm.init
            { description = ""
            , amount = ""
            , payor = Just session.id
            , beneficients = Set.empty
            , tags = []
            }
    , saveState = Composing
    }



-- UPDATE


type Msg
    = GotComposingFormMsg TransactionComposeForm.Msg
    | SaveTransaction TransactionEdit
    | SaveTransaction_Response (RemoteData (Graphql.Http.Error TransactionId) TransactionId)
    | OpenSavedTransaction
    | AddAnotherOne


update : Context msg -> Msg -> ( ( Model, Cmd Msg ), Cmd GlobalMsg )
update ctx msg =
    let
        { model, session } =
            ctx
    in
    case msg of
        GotComposingFormMsg subMsg ->
            let
                subCtx =
                    { model = model.form
                    , lift = GotComposingFormMsg
                    , todayDate = ctx.todayDate
                    , commonData = ctx.commonData
                    , session = ctx.session
                    }

                ( ( newModelForm, nextSubMsg ), globalMsg, events ) =
                    TransactionComposeForm.update subCtx subMsg

                newModel =
                    { model | form = newModelForm }

                passEvents : List (Cmd Msg)
                passEvents =
                    List.foldl
                        (\evt evts ->
                            (case evt of
                                OnSaveTransaction t ->
                                    Cmd.Extra.perform <| SaveTransaction t
                            )
                                :: evts
                        )
                        []
                        events

                cmds =
                    Cmd.batch <| Cmd.map GotComposingFormMsg nextSubMsg :: passEvents
            in
            newModel |> Cmd.Extra.with cmds |> Cmd.Extra.with globalMsg

        SaveTransaction transactionEdit ->
            let
                newTransaction : NewTransaction
                newTransaction =
                    transactionEdit

                sendReqCmd =
                    newTransaction
                        |> addTransaction
                        |> sendMutationRequest SaveTransaction_Response
            in
            ( { model | saveState = Saving }, sendReqCmd ) |> noCmd

        SaveTransaction_Response result ->
            case result of
                RemoteData.Failure error ->
                    { model | saveState = SaveError } |> noCmd |> noCmd

                RemoteData.Success transactionId ->
                    { model | saveState = Saved transactionId } |> noCmd |> noCmd

                _ ->
                    model |> noCmd |> noCmd

        OpenSavedTransaction ->
            case model.saveState of
                Saved transactionId ->
                    ( model |> noCmd
                    , Cmd.Extra.perform (Navigate <| Route.Transaction transactionId)
                    )

                _ ->
                    model |> noCmd |> noCmd

        AddAnotherOne ->
            init session |> noCmd



-- VIEW


view : Context msg -> Element msg
view ctx =
    let
        { model } =
            ctx

        composeFormViewState : TransactionComposeForm.ViewState
        composeFormViewState =
            case model.saveState of
                Composing ->
                    EditComposing

                Saving ->
                    EditSaving

                SaveError ->
                    EditSaving

                Saved transactionId ->
                    EditDone
    in
    case model.saveState of
        Saved transactionId ->
            column [ paddingXY 0 30, spacing 20, centerX ]
                [ row [ centerX ] [ Element.html <| successAnim { diameter = 150, fillColor = "teal", strokeColor = "green" } ]
                , Input.button
                    [ Bg.color Colors.white
                    , Font.color Colors.black
                    , paddingXY 12 6
                    , Border.rounded 3
                    , Border.width 1
                    , Border.color Colors.teal500
                    , centerX
                    ]
                    { onPress = Just <| ctx.lift AddAnotherOne
                    , label = text "Success! One more?"
                    }
                ]

        _ ->
            let
                formCtx =
                    { model = model.form
                    , commonData = ctx.commonData
                    , todayDate = ctx.todayDate
                    , lift = ctx.lift << GotComposingFormMsg
                    , session = ctx.session
                    }
            in
            TransactionComposeForm.viewForm formCtx composeFormViewState
