module Views.TransactionComposeForm exposing (Command(..), Context, Event(..), Model, Msg, ViewState(..), command, init, update, viewForm)

import Browser.Dom as Dom
import Data.Context exposing (ContextData, GlobalMsg, Logged, passContextWithoutSession)
import Data.Person exposing (Person, PersonId)
import Data.Transaction exposing (Transaction, TransactionEdit)
import Element exposing (Element, alignLeft, alignRight, below, centerX, centerY, column, el, explain, fill, fillPortion, focused, height, inFront, mouseDown, moveDown, moveLeft, moveUp, padding, paddingXY, paragraph, px, rgb255, rgba255, row, shrink, spaceEvenly, spacing, text, width, wrappedRow)
import Element.Background as Bg
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Html.Events
import Json.Decode as Json
import List.Extra
import Misc exposing (attr, attrWhen, either, moneyRegex, noCmd, noShadow, pairToTriple, userSelectNone, viewIcon, viewIconButton, viewIf)
import Misc.Colors as Colors exposing (blue500, rgbHex, teal100, teal500, teal700, white)
import Regex
import Set exposing (Set)
import Task
import Views.ChipsTextfield as ChipsTextfield


type alias Context msg =
    Logged (ContextData Model Msg msg)


type alias TransactionComposed =
    { description : String
    , amount : String
    , payor : Maybe PersonId
    , beneficients : Set PersonId
    , tags : List String
    }


type alias Model =
    { transaction : TransactionComposed
    , tags : ChipsTextfield.Model
    }


init : TransactionComposed -> Model
init transaction =
    { transaction = transaction
    , tags = ChipsTextfield.init transaction.tags
    }


type Command
    = DoFocusAmountInput


type Event
    = OnSaveTransaction TransactionEdit


command : Command -> Msg
command =
    GotExternalCommand


type Msg
    = GotExternalCommand Command
    | SelectPayor PersonId
    | ToggleBeneficient PersonId Bool
    | SetAmount String
    | SetDescription String
    | ElementFocused (Result Dom.Error ())
    | OnPaymentAmountKeyDown Int
    | OnDescriptionKeyDown Int
    | TagsMsg ChipsTextfield.Msg
    | ToggleTag String
    | SaveTransaction


update : Context msg -> Msg -> ( ( Model, Cmd Msg ), Cmd GlobalMsg, List Event )
update ctx msg =
    let
        { model } =
            ctx

        transaction =
            model.transaction
    in
    case msg of
        GotExternalCommand cmd ->
            case cmd of
                DoFocusAmountInput ->
                    ( model, Dom.focus idsStr.paymentAmount |> Task.attempt ElementFocused )
                        |> noCmd
                        |> noEvt

        SelectPayor personId ->
            { model | transaction = { transaction | payor = Just personId, beneficients = Set.empty } }
                |> noCmd
                |> noCmd
                |> noEvt

        ToggleBeneficient personId isSelected ->
            { model
                | transaction =
                    { transaction
                        | beneficients =
                            if isSelected then
                                Set.insert personId transaction.beneficients

                            else
                                Set.remove personId transaction.beneficients
                    }
            }
                |> noCmd
                |> noCmd
                |> noEvt

        SetAmount amountStr ->
            let
                newTransaction =
                    case amountStrToInt amountStr of
                        0 ->
                            if amountStr == "" then
                                { transaction | amount = "" }

                            else
                                transaction

                        newAmount ->
                            { transaction | amount = amountStr }
            in
            { model | transaction = newTransaction } |> noCmd |> noCmd |> noEvt

        SetDescription str ->
            { model | transaction = { transaction | description = str } } |> noCmd |> noCmd |> noEvt

        ElementFocused res ->
            model |> noCmd |> noCmd |> noEvt

        OnPaymentAmountKeyDown keyCode ->
            let
                cmd =
                    if keyCode == 13 then
                        Dom.focus idsStr.description |> Task.attempt ElementFocused

                    else
                        Cmd.none
            in
            ( model, cmd ) |> noCmd |> noEvt

        OnDescriptionKeyDown keyCode ->
            let
                cmds =
                    if keyCode == 13 then
                        [ Dom.focus "input_add-tag" |> Task.attempt ElementFocused ]

                    else
                        []
            in
            ( model, Cmd.batch cmds ) |> noCmd |> noEvt

        TagsMsg tagsMsg ->
            let
                ( subModel, subMsg ) =
                    ChipsTextfield.update tagsMsg model.tags
            in
            ( { model | tags = subModel }, Cmd.map TagsMsg subMsg ) |> noCmd |> noEvt

        ToggleTag tagName ->
            let
                tagExists =
                    List.member tagName model.tags.chips

                newChips =
                    if tagExists then
                        List.Extra.remove tagName model.tags.chips

                    else
                        tagName :: model.tags.chips

                tags =
                    model.tags

                newTags =
                    { tags | chips = newChips }
            in
            { model | tags = newTags } |> noCmd |> noCmd |> noEvt

        SaveTransaction ->
            let
                amountAsNum =
                    amountStrToInt model.transaction.amount

                maybeTransaction : Maybe TransactionEdit
                maybeTransaction =
                    case ( transaction.payor, amountAsNum ) of
                        ( _, 0 ) ->
                            Nothing

                        ( Nothing, _ ) ->
                            Nothing

                        ( Just payorId, amount ) ->
                            { amount = amount
                            , description = Just transaction.description

                            -- , insertedAt = Nothing
                            , payorId = payorId
                            , beneficientIds = Set.toList transaction.beneficients
                            , tags = Just model.tags.chips
                            }
                                |> Just

                events =
                    maybeTransaction
                        |> Maybe.map OnSaveTransaction
                        |> Maybe.map (\val -> [ val ])
                        |> Maybe.withDefault []
                        |> Debug.log "events"
            in
            model |> noCmd |> noCmd |> withEvts events


withEvts : List Event -> ( a, Cmd msg ) -> ( a, Cmd msg, List Event )
withEvts evt ( model, cmds ) =
    pairToTriple evt ( model, cmds )


noEvt : ( a, Cmd msg ) -> ( a, Cmd msg, List Event )
noEvt ( model, cmds ) =
    pairToTriple [] ( model, cmds )


{-| IDs useful for focusing visual elements.
-}
idsStr =
    { paymentAmount = "elm-payment-amount"
    , description = "elm-payment-description"
    }


amountStrToInt : String -> Int
amountStrToInt str =
    case Regex.find moneyRegex str of
        [ m ] ->
            if m.index == 0 && String.length m.match == String.length str then
                String.toFloat m.match
                    |> Maybe.andThen (\a -> Just <| floor (a * 100.0))
                    |> Maybe.withDefault 0

            else
                0

        _ ->
            0


isFormFilled : Model -> Bool
isFormFilled model =
    let
        transaction =
            model.transaction
    in
    transaction.payor
        |> Maybe.map
            (\payorId ->
                if amountStrToInt transaction.amount /= 0 && (Set.size transaction.beneficients > 0) then
                    not <| Set.size transaction.beneficients == 1 && Set.member payorId transaction.beneficients

                else
                    False
            )
        |> Maybe.withDefault False



--
--isFormDisabled : Model -> Bool
--isFormDisabled model =
--    not <| isFormEnabled model
--
--
--isFormEnabled : Model -> Bool
--isFormEnabled model =
--    case model.saveState of
--        Composing ->
--            True
--
--        Saving ->
--            False
--
--        SaveError ->
--            True
--
--        Saved ->
--            False
-- VIEW


type ViewState
    = EditComposing
    | EditSaving
    | EditDone


viewForm : Context msg -> ViewState -> Element msg
viewForm ctx viewState =
    column
        [ width fill
        , userSelectNone
        , Border.rounded 2
        , Border.color <| teal700
        , Border.solid
        , Border.width 1
        , attrWhen (viewState == EditSaving) <|
            inFront <|
                column [ width fill, height fill, Bg.color <| Colors.rgbaHex 0 0.7 ] []
        ]
        [ paragraph
            [ Bg.color teal500
            , Font.color white
            , Border.shadow { offset = ( 0, 2 ), size = 0, blur = 10, color = rgba255 0 0 0 0.4 }
            , paddingXY 22 18
            , inFront <|
                column [ width fill, moveDown 10, moveLeft 22 ]
                    [ column [ alignRight ] [ viewSave ctx viewState ] ]
            , below <|
                viewIf (viewState == EditSaving) <|
                    row [ moveUp 5, height (px 10), width fill ] [ Misc.viewLoadingBar ]
            ]
            [ text "New transaction" ]
        , row
            [ width fill
            , spacing 30
            , padding 10
            ]
            [ column [ paddingXY 10 15, spacing 30, width fill ]
                [ viewAmount ctx
                , viewMoneyDirection ctx
                , viewDescription ctx viewState
                , viewTags ctx viewState
                ]
            , el [ alignRight ] <| viewUsualTags ctx
            ]
        ]


viewAmount : Context msg -> Element msg
viewAmount ctx =
    let
        { model } =
            ctx
    in
    Input.text
        [ Element.htmlAttribute <| Html.Events.on "keydown" (Json.map (ctx.lift << OnPaymentAmountKeyDown) Html.Events.keyCode)
        , attr "id" idsStr.paymentAmount
        , attr "type" "number"
        ]
        { onChange = ctx.lift << SetAmount
        , label = Input.labelAbove [] Element.none
        , text = model.transaction.amount
        , placeholder = Just (Input.placeholder [] <| text "Money amount (PLN)")
        }


payorSelection : Context msg -> Element msg
payorSelection ctx =
    let
        { model } =
            ctx
    in
    column [ spacing 15 ]
        [ Input.radio []
            { label = Input.labelAbove [] <| Element.none
            , onChange = ctx.lift << SelectPayor
            , options =
                ctx.commonData.people
                    |> List.map
                        (\person ->
                            Input.option person.id (el [ paddingXY 0 5 ] <| text person.name)
                        )
            , selected = model.transaction.payor
            }
        ]


beneficientSelection : Context msg -> Element msg
beneficientSelection ctx =
    let
        { model } =
            ctx

        viewEl : Int -> Person -> Element msg
        viewEl idx person =
            let
                isSelected =
                    model.transaction.beneficients |> Set.member person.id
            in
            Input.checkbox []
                { onChange = ctx.lift << ToggleBeneficient person.id
                , checked = isSelected
                , icon = either "check" "check-empty" >> viewIcon []
                , label = Input.labelRight [] <| text person.name
                }
    in
    column [ spacing 8 ]
        (ctx.commonData.people
            |> List.indexedMap viewEl
        )


viewMoneyDirection : Context msg -> Element msg
viewMoneyDirection ctx =
    let
        { model } =
            ctx
    in
    row [ width fill ]
        [ Element.el [ width <| fillPortion 1 ] <| payorSelection ctx
        , Element.el [ width <| fillPortion 1, Font.center ] (text "⇢")
        , Element.el [ width <| fillPortion 1 ] <| beneficientSelection ctx
        ]


viewDescription : Context msg -> ViewState -> Element msg
viewDescription ctx viewState =
    let
        { model } =
            ctx
    in
    Input.text
        [ width fill
        , attr "id" idsStr.description
        , Element.htmlAttribute <| Html.Events.on "keydown" (Json.map (ctx.lift << OnDescriptionKeyDown) Html.Events.keyCode)
        ]
        { onChange = ctx.lift << SetDescription
        , placeholder =
            Just
                (Input.placeholder [] <|
                    (text <|
                        if viewState /= EditComposing then
                            if String.isEmpty model.transaction.description then
                                "No description"

                            else
                                "Description"

                        else
                            "Description (optional)"
                    )
                )
        , text = model.transaction.description
        , label = Input.labelHidden ""
        }


viewTags : Context msg -> ViewState -> Element msg
viewTags ctx viewState =
    let
        subContext =
            passContextWithoutSession ctx .tags (ctx.lift << TagsMsg)

        text =
            if viewState /= EditComposing then
                "Tags (optional)"

            else
                "Tags"

        cfg =
            ChipsTextfield.Config (viewState == EditComposing) text "No tags were selected."
    in
    ChipsTextfield.view subContext cfg


viewUsualTags : Context msg -> Element msg
viewUsualTags ctx =
    let
        iconizedTags =
            [ ( "wycieczka", "flight" )
            , ( "narzędzia", "wrench" )
            , ( "auto", "cab" )
            , ( "gotówka", "wallet" )
            , ( "jedzenie", "food" )
            , ( "mieszkanie", "home" )
            ]
    in
    column [ spacing 15 ] <|
        List.indexedMap (viewUsualTag ctx) iconizedTags


viewUsualTag : Context msg -> Int -> ( String, String ) -> Element msg
viewUsualTag ctx idx ( tagName, iconName ) =
    let
        selected =
            List.member tagName ctx.model.tags.chips
    in
    viewIconButton
        [ width (px 34)
        , height (px 34)
        , Border.rounded 100
        , Bg.color teal100
        , Bg.color teal500 |> attrWhen selected
        ]
        iconName
        (ctx.lift <| ToggleTag tagName)


viewSave : Context msg -> ViewState -> Element msg
viewSave ctx viewState =
    let
        { model } =
            ctx

        btnText =
            "Save"

        isEnabled =
            isFormFilled model && (viewState == EditComposing)
    in
    Input.button
        [ Bg.color blue500
        , Font.color white
        , paddingXY 12 6
        , Border.rounded 2
        , Border.width 1
        , Border.color Colors.gray50 |> attrWhen isEnabled
        , mouseDown [ noShadow ]
        , mouseDown [ Font.color teal100 ] |> attrWhen isEnabled
        , focused [ noShadow ]
        , Font.color Colors.blueGray900 |> attrWhen (not isEnabled)
        ]
        { onPress =
            isEnabled
                |> either
                    (Just <| ctx.lift SaveTransaction)
                    Nothing
        , label = text btnText
        }
