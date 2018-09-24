module Page.NewTransaction exposing (Model, Msg, init, update, view)

import Browser.Dom as Dom
import Cmd.Extra
import Data.Context exposing (..)
import Data.Person exposing (Person, PersonId)
import Data.Session exposing (Session)
import Data.Transaction exposing (TransactionId)
import Element exposing (Element, alignLeft, alignRight, centerX, centerY, column, el, explain, fill, height, padding, paddingXY, paragraph, px, rgb255, row, spaceEvenly, spacing, text, width, wrappedRow)
import Element.Background as Bg
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import GraphQL.Client.Http
import Html.Attributes as Attr
import Html.Events exposing (keyCode)
import Json.Decode as Json
import Json.Encode
import List
import List.Extra
import Maybe.Extra exposing (isJust, isNothing)
import Misc exposing (attrWhen, black, either, match, moneyRegex, noCmd, teal100, teal500, toggle, userSelectNone, viewIcon, viewIconButton, viewIf)
import Regex
import Request.AddTransaction exposing (..)
import Request.Common exposing (..)
import Request.People exposing (..)
import Route
import Set exposing (Set)
import Styles.Common exposing (csg)
import Styles.NewTransaction exposing (..)
import Task
import Views.ChipsTextfield as ChipsTextfield


type alias Model =
    { paymentDescription : String
    , amount : String
    , payor : Maybe PersonId
    , payees : Set PersonId
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
            { paymentDescription = ""
            , amount = ""
            , payor = Just session.user.id
            , payees = Set.empty
            , people = []
            , tags = ChipsTextfield.init
            , saveState = Composing
            }
    in
    ( model, Cmd.Extra.perform RefreshPeopleList )


type Msg
    = RefreshPeopleList
    | RefreshPeopleListResponse (Result GraphQL.Client.Http.Error (List Person))
    | SelectPayor PersonId
    | TogglePayee PersonId Bool
    | SetAmount String
    | SetPaymentDescription String
    | ElementFocused (Result Dom.Error ())
    | OnPaymentAmountKeyDown Int
    | OnPaymentDescriptionKeyDown Int
    | TagsMsg ChipsTextfield.Msg
    | ToggleTag String
    | SaveTransaction
    | SaveTransactionResponse (Result GraphQL.Client.Http.Error TransactionId)
    | OpenSavedTransaction


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
                    getPeople
                        |> sendQueryRequest
                        |> Task.attempt RefreshPeopleListResponse
            in
            ( model, cmd ) |> noCmd

        RefreshPeopleListResponse result ->
            let
                newModel =
                    result
                        |> Result.andThen (\people -> Ok { model | people = people })
                        |> Result.withDefault model
            in
            ( newModel, Dom.focus idsStr.paymentAmount |> Task.attempt ElementFocused )
                |> noCmd

        SelectPayor personId ->
            { model | payor = Just personId, payees = Set.empty } |> noCmd |> noCmd

        TogglePayee personId isSelected ->
            { model
                | payees =
                    if isSelected then
                        Set.insert personId model.payees

                    else
                        Set.remove personId model.payees
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
                        [ Cmd.Extra.perform SaveTransaction ]

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

                            -- , paidAt = Nothing
                            , payorId = payorId
                            , payeeIds = Set.toList model.payees
                            , tags = Just model.tags.chips
                            }
                                |> Just

                sendReqCmd =
                    newTransaction
                        |> Maybe.andThen (Just << addTransaction)
                        |> Maybe.andThen (Just << sendMutationRequest)
            in
            case sendReqCmd of
                Just cmd ->
                    ( { model | saveState = Saving }
                    , Task.attempt SaveTransactionResponse cmd
                    )
                        |> noCmd

                _ ->
                    model |> noCmd |> noCmd

        SaveTransactionResponse result ->
            (case result of
                Err error ->
                    { model | saveState = SaveError }

                Ok res ->
                    { model | saveState = Saved (SaveResult res) }
            )
                |> noCmd
                |> noCmd

        OpenSavedTransaction ->
            case model.saveState of
                Saved { paymentId } ->
                    ( model |> noCmd
                    , Cmd.Extra.perform (Navigate <| Route.Transaction paymentId)
                    )

                _ ->
                    model |> noCmd |> noCmd


{-| IDs useful for focusing visual elements.
-}
idsStr =
    { paymentAmount = "elm-payment-amount"
    , paymentDescription = "elm-payment-description"
    }


{-| IDs needed for Material UI to conveniently distinguish states of components.
-}



-- ids =
--     { paymentDescription = 0
--     , amount = 1
--     , btnAdd = 2
--     , catSel = 3
--     , payorSelection = 4
--     , payeeSelection = 5
--     , tags = 6
--     , usualTags = 7
--     , btnOpenSavedPayment = 8
--     }


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
    amountStrToInt model.amount /= 0


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

        formDisabled =
            isFormDisabled model
    in
    column
        [ width fill
        , userSelectNone
        ]
        [ paragraph [] [ text "New transaction" ]
        , row [ width fill, spacing 30 ]
            [ column [ paddingXY 10 15, spacing 30, width fill ]
                [ viewAmount ctx
                , viewMoneyDirection ctx
                , viewDescription ctx
                , viewTags ctx
                ]
            , el [ alignRight ] <| viewUsualTags ctx
            ]
        , viewSave ctx
        , viewSaveResult ctx |> viewIf (model.saveState /= Composing)
        ]


viewAmount : Context msg -> Element msg
viewAmount ctx =
    let
        { model } =
            ctx
    in
    Input.text []
        { onChange = ctx.lift << SetAmount
        , label = Input.labelAbove [] Element.none
        , text = model.amount
        , placeholder = Just (Input.placeholder [] <| text "Money amount (PLN)")
        }



-- [ Attr.type_ "number"
-- , Attr.property "pattern" (Json.Encode.string "[0-9]*([,.][0-9]{2})?")
-- , Attr.value <| Maybe.withDefault "" model.amount
-- , Attr.id idsStr.paymentAmount
-- , Attr.class <| cls Amount
-- , Attr.placeholder "PLN"
-- , Html.Events.onInput (ctx.lift << SetAmount)
-- , Html.Events.on "keydown" (Json.map (ctx.lift << OnPaymentAmountKeyDown) Html.Events.keyCode)
-- ]
-- []
{- Textfield.render ctx.liftMaterial
   [ ids.amount ]
   ctx.mdl
   [ Textfield.label "Amount"
   , Textfield.floatingLabel
   , Opts.onInput (ctx.lift << SetAmount)
   , Opts.on "keydown" (Json.map (ctx.lift << OnPaymentAmountKeyDown) Html.Events.keyCode)
   , Textfield.disabled |> Opts.when (isFormDisabled model)
   , Textfield.value (Maybe.withDefault "" model.amount)
   , Textfield.error "Not numeric!"
       |> Opts.when
           (Maybe.Extra.unwrap False
               (not << match moneyRegex)
               model.amount
           )
   , Opts.id idsStr.paymentAmount
   , cs Amount
   ]
   []
-}


payorSelection : Context msg -> Element msg
payorSelection ctx =
    let
        { model } =
            ctx

        viewEl : Int -> Person -> Element msg
        viewEl idx person =
            let
                isSelected =
                    model.payor
                        |> Maybe.Extra.unwrap False (\pid -> pid == person.id)
            in
            row
                [-- Opts.onClick (ctx.lift <| SelectPayor person.id) |> Opts.when (isFormEnabled model)
                 -- , csg "interactive" |> Opts.cs |> Opts.when (isFormEnabled model)
                ]
                [ paragraph [] [ Element.el [] (text person.name) ] ]

        -- [ Input.radioRow  { onChange : option -> msg, options : List.List (Element.Input.Option option msg), selected : Maybe.Maybe option, label : Element.Input.Label msg }
        --         [ Toggles.value isSelected
        --         , Toggles.group "payorSelection"
        --         , Opts.onToggle (ctx.lift <| SelectPayor person.id) |> Opts.when (isFormEnabled model)
        --         , Toggles.ripple |> Opts.when (isFormEnabled model)
        --         ]
        --         []
        --     ]
        -- , text person.name
        -- ]
    in
    column [ spacing 15 ]
        (ctx.model.people |> List.indexedMap viewEl)


payeeSelection : Context msg -> Element msg
payeeSelection ctx =
    let
        { model } =
            ctx

        viewEl : Int -> Person -> Element msg
        viewEl idx person =
            let
                isSelected =
                    model.payees |> Set.member person.id
            in
            Input.checkbox []
                { onChange = ctx.lift << TogglePayee person.id
                , checked = isSelected
                , icon = either "check" "check-empty" >> viewIcon []
                , label = Input.labelRight [] <| text person.name
                }
    in
    row []
        (ctx.model.people
            |> List.filter
                (\p ->
                    Just p.id /= model.payor
                )
            |> List.indexedMap viewEl
        )


viewMoneyDirection : Context msg -> Element msg
viewMoneyDirection ctx =
    let
        { model } =
            ctx
    in
    row [ width fill, spaceEvenly ]
        [ payorSelection ctx
        , Element.el
            [ centerY ]
            (text "⇢")
        , payeeSelection ctx
        ]


viewDescription : Context msg -> Element msg
viewDescription ctx =
    let
        { model } =
            ctx
    in
    Input.text
        [ width fill

        -- Opts.on "keydown" (Json.map (ctx.lift << OnPaymentDescriptionKeyDown) Html.Events.keyCode)
        ]
        { onChange = ctx.lift << SetPaymentDescription
        , placeholder = Nothing
        , label =
            Input.labelAbove []
                (text <|
                    if isFormDisabled model then
                        if String.isEmpty model.paymentDescription then
                            "No description"

                        else
                            "Description"

                    else
                        "Description (optional)"
                )
        , text = model.paymentDescription
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
            [ ( "journey", "flight" )
            , ( "tool", "wrench" )
            , ( "car", "cab" )
            , ( "cash", "wallet" )
            , ( "food", "food" )
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
        {- TODO: Save transfer/shared payment -}
        btnText =
            "Save"
    in
    row [ padding 15 ]
        [ Input.button []
            { onPress =
                (isFormFilled ctx.model
                    && (not <| isFormDisabled ctx.model)
                )
                    |> either
                        (Just <| ctx.lift SaveTransaction)
                        Nothing
            , label = text btnText
            }
        ]


viewSaveResult : Context msg -> Element msg
viewSaveResult ctx =
    let
        { model } =
            ctx
    in
    row
        [ paddingXY 20 39 |> attrWhen (model.saveState /= Saving)
        ]
        [ case model.saveState of
            Composing ->
                text ""

            Saving ->
                paragraph [] [ text "Saving..." ]

            SaveError ->
                paragraph
                    [ Font.bold, Font.color (rgb255 229 57 53) ]
                    [ text "Some error occured, please try again or contact Administrator." ]

            Saved result ->
                Input.button []
                    { onPress = Just <| ctx.lift OpenSavedTransaction
                    , label = text "Open Payment"
                    }
        ]
