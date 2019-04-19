module Page.NewTransaction exposing (Model, Msg, init, update, view)

import Browser.Dom as Dom
import Cmd.Extra
import Data.Context exposing (..)
import Data.Person exposing (Person, PersonId)
import Data.Session exposing (Session)
import Data.Transaction exposing (TransactionId)
import Element exposing (Element, alignLeft, alignRight, below, centerX, centerY, column, el, explain, fill, fillPortion, focused, height, inFront, mouseDown, moveDown, moveLeft, moveUp, padding, paddingXY, paragraph, px, rgb255, rgba255, row, shrink, spaceEvenly, spacing, text, width, wrappedRow)
import Element.Background as Bg
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input exposing (labelRight)
import Graphql.Http
import Html.Attributes as Attr
import Html.Events exposing (keyCode)
import Json.Decode as Json
import Json.Encode
import List
import List.Extra
import Maybe.Extra exposing (isJust, isNothing)
import Misc exposing (attr, attrWhen, either, match, moneyRegex, noCmd, noShadow, userSelectNone, viewIcon, viewIconButton, viewIf)
import Misc.Colors as Colors exposing (blue500, rgbHex, teal100, teal500, teal700, white)
import Misc.SuccessAnimation exposing (successAnim)
import Regex
import RemoteData exposing (RemoteData)
import Request.AddTransaction exposing (..)
import Request.Common exposing (..)
import Request.People exposing (..)
import Route
import Set exposing (Set)
import Task
import Views.ChipsTextfield as ChipsTextfield


type alias Model =
    { paymentDescription : String
    , amount : String
    , payor : Maybe PersonId
    , beneficients : Set PersonId
    , people : List Person
    , tags : ChipsTextfield.Model
    , saveState : SaveState
    }


type SaveState
    = Composing
    | Saving
    | SaveError
    | Saved SaveResult


type alias SaveResult =
    { paymentId : TransactionId }


init : Session -> ( Model, Cmd Msg )
init session =
    let
        model =
            initModel session
    in
    ( model, Cmd.Extra.perform RefreshPeopleList )


initModel : Session -> Model
initModel session =
    { paymentDescription = ""
    , amount = ""
    , payor = Just session.id
    , beneficients = Set.empty
    , people = []
    , tags = ChipsTextfield.init
    , saveState = Composing
    }


type Msg
    = RefreshPeopleList
    | RefreshPeopleList_Response (RemoteData (Graphql.Http.Error (List Person)) (List Person))
    | SelectPayor PersonId
    | ToggleBeneficient PersonId Bool
    | SetAmount String
    | SetPaymentDescription String
    | ElementFocused (Result Dom.Error ())
    | OnPaymentAmountKeyDown Int
    | OnPaymentDescriptionKeyDown Int
    | TagsMsg ChipsTextfield.Msg
    | ToggleTag String
    | SaveTransaction
    | SaveTransaction_Response (RemoteData (Graphql.Http.Error TransactionId) TransactionId)
    | OpenSavedTransaction
    | AddAnotherOne


type alias Context msg =
    Logged (ContextData Model Msg msg)


update : Context msg -> Msg -> ( ( Model, Cmd Msg ), Cmd GlobalMsg )
update ctx msg =
    let
        { model, session } =
            ctx
    in
    case msg of
        RefreshPeopleList ->
            let
                cmd =
                    getPeople |> sendQueryRequest RefreshPeopleList_Response
            in
            ( model, cmd ) |> noCmd

        RefreshPeopleList_Response result ->
            let
                newModel =
                    result
                        |> RemoteData.andThen (\people -> RemoteData.Success { model | people = people })
                        |> RemoteData.withDefault model
            in
            ( newModel, Dom.focus idsStr.paymentAmount |> Task.attempt ElementFocused )
                |> noCmd

        SelectPayor personId ->
            { model | payor = Just personId, beneficients = Set.empty } |> noCmd |> noCmd

        ToggleBeneficient personId isSelected ->
            { model
                | beneficients =
                    if isSelected then
                        Set.insert personId model.beneficients

                    else
                        Set.remove personId model.beneficients
            }
                |> noCmd
                |> noCmd

        SetAmount amountStr ->
            let
                newModel =
                    case amountStrToInt amountStr of
                        0 ->
                            if amountStr == "" then
                                { model | amount = "" }

                            else
                                model

                        newAmount ->
                            { model | amount = amountStr }
            in
            newModel |> noCmd |> noCmd

        SetPaymentDescription str ->
            { model | paymentDescription = str } |> noCmd |> noCmd

        ElementFocused res ->
            model |> noCmd |> noCmd

        OnPaymentAmountKeyDown keyCode ->
            let
                cmd =
                    if keyCode == 13 then
                        Dom.focus idsStr.paymentDescription |> Task.attempt ElementFocused

                    else
                        Cmd.none
            in
            ( model, cmd ) |> noCmd

        OnPaymentDescriptionKeyDown keyCode ->
            let
                cmds =
                    if keyCode == 13 then
                        [ Dom.focus "input_add-tag" |> Task.attempt ElementFocused ]

                    else
                        []
            in
            ( model, Cmd.batch cmds ) |> noCmd

        TagsMsg tagsMsg ->
            let
                ( subModel, subMsg ) =
                    ChipsTextfield.update tagsMsg model.tags
            in
            ( { model | tags = subModel }, Cmd.map TagsMsg subMsg ) |> noCmd

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
            { model | tags = newTags } |> noCmd |> noCmd

        SaveTransaction ->
            let
                amountAsNum =
                    amountStrToInt model.amount

                newTransaction : Maybe NewTransaction
                newTransaction =
                    case ( model.payor, amountAsNum ) of
                        ( _, 0 ) ->
                            Nothing

                        ( Nothing, _ ) ->
                            Nothing

                        ( Just payorId, amount ) ->
                            { amount = amount
                            , description = Just model.paymentDescription

                            -- , insertedAt = Nothing
                            , payorId = payorId
                            , beneficientIds = Set.toList model.beneficients
                            , tags = Just model.tags.chips
                            }
                                |> Just

                sendReqCmd =
                    newTransaction
                        |> Maybe.andThen (Just << addTransaction)
                        |> Maybe.andThen (Just << sendMutationRequest SaveTransaction_Response)
            in
            case sendReqCmd of
                Just cmd ->
                    ( { model | saveState = Saving }
                    , cmd
                    )
                        |> noCmd

                _ ->
                    model |> noCmd |> noCmd

        SaveTransaction_Response result ->
            case result of
                RemoteData.Failure error ->
                    { model | saveState = SaveError } |> noCmd |> noCmd

                RemoteData.Success res ->
                    { model | saveState = Saved (SaveResult res) } |> noCmd |> noCmd

                _ ->
                    model |> noCmd |> noCmd

        OpenSavedTransaction ->
            case model.saveState of
                Saved { paymentId } ->
                    ( model |> noCmd
                    , Cmd.Extra.perform (Navigate <| Route.Transaction paymentId)
                    )

                _ ->
                    model |> noCmd |> noCmd

        AddAnotherOne ->
            let
                newModel =
                    initModel session
            in
            { newModel | people = model.people } |> noCmd |> noCmd


{-| IDs useful for focusing visual elements.
-}
idsStr =
    { paymentAmount = "elm-payment-amount"
    , paymentDescription = "elm-payment-description"
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
    model.payor
        |> Maybe.map
            (\payorId ->
                if amountStrToInt model.amount /= 0 && (Set.size model.beneficients > 0) then
                    not <| Set.size model.beneficients == 1 && Set.member payorId model.beneficients

                else
                    False
            )
        |> Maybe.withDefault False


isFormDisabled : Model -> Bool
isFormDisabled model =
    not <| isFormEnabled model


isFormEnabled : Model -> Bool
isFormEnabled model =
    case model.saveState of
        Composing ->
            True

        Saving ->
            False

        SaveError ->
            True

        Saved saveResult ->
            False



-- VIEW


view : Context msg -> Element msg
view ctx =
    let
        { model } =
            ctx
    in
    case model.saveState of
        Saved result ->
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
            viewForm ctx


viewForm : Context msg -> Element msg
viewForm ctx =
    let
        { model } =
            ctx

        formDisabled =
            isFormDisabled model
    in
    column
        [ width fill
        , userSelectNone
        , Border.rounded 2
        , Border.color <| teal700
        , Border.solid
        , Border.width 1
        , attrWhen (model.saveState == Saving) <|
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
                    [ column [ alignRight ] [ viewSave ctx ] ]
            , below <|
                viewIf (model.saveState == Saving) <|
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
                , viewDescription ctx
                , viewTags ctx
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
        , text = model.amount
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
                model.people
                    |> List.map
                        (\person ->
                            Input.option person.id (el [ paddingXY 0 5 ] <| text person.name)
                        )
            , selected = model.payor
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
                    model.beneficients |> Set.member person.id
            in
            Input.checkbox []
                { onChange = ctx.lift << ToggleBeneficient person.id
                , checked = isSelected
                , icon = either "check" "check-empty" >> viewIcon []
                , label = Input.labelRight [] <| text person.name
                }
    in
    column [ spacing 8 ]
        (ctx.model.people
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


viewDescription : Context msg -> Element msg
viewDescription ctx =
    let
        { model } =
            ctx
    in
    Input.text
        [ width fill
        , attr "id" idsStr.paymentDescription
        , Element.htmlAttribute <| Html.Events.on "keydown" (Json.map (ctx.lift << OnPaymentDescriptionKeyDown) Html.Events.keyCode)
        ]
        { onChange = ctx.lift << SetPaymentDescription
        , placeholder =
            Just
                (Input.placeholder [] <|
                    (text <|
                        if isFormDisabled model then
                            if String.isEmpty model.paymentDescription then
                                "No description"

                            else
                                "Description"

                        else
                            "Description (optional)"
                    )
                )
        , text = model.paymentDescription
        , label = Input.labelHidden ""
        }


viewTags : Context msg -> Element msg
viewTags ctx =
    let
        subContext =
            passContextWithoutSession ctx .tags (ctx.lift << TagsMsg)

        text =
            if isFormDisabled ctx.model then
                "Tags (optional)"

            else
                "Tags"

        cfg =
            ChipsTextfield.Config (isFormEnabled ctx.model) text "No tags were selected."
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


viewSave : Context msg -> Element msg
viewSave ctx =
    let
        { model } =
            ctx

        btnText =
            "Save"

        isEnabled =
            isFormFilled ctx.model && (not <| isFormDisabled ctx.model)
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