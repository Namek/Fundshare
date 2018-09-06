module Page.NewTransaction exposing (Model, Msg, init, update, view)

import Cmd.Extra
import Data.Context exposing (..)
import Data.Person exposing (Person, PersonId)
import Data.Session exposing (Session)
import Data.Transaction exposing (TransactionId)
import Dom
import GraphQL.Client.Http
import Html exposing (..)
import Html.Attributes as Attr
import Html.Events exposing (keyCode)
import Json.Decode as Json
import Json.Encode
import List
import List.Extra
import Material.Button as Button
import Material.Color as Color
import Material.Elevation as Elevation
import Material.Icon as Icon
import Material.List as Lists
import Material.Options as Opts exposing (css)
import Material.Progress as Loading
import Material.Textfield as Textfield
import Material.Toggles as Toggles
import Material.Typography as Typo
import Maybe.Extra exposing (isJust, isNothing)
import Misc exposing ((=>), match, moneyRegex, viewIf)
import Request.AddTransaction exposing (..)
import Request.Common exposing (..)
import Request.People exposing (..)
import Route
import Set exposing (Set)
import Set.Extra
import Styles.Common exposing (csg)
import Styles.NewTransaction exposing (..)
import Task
import Views.ChipsTextfield as ChipsTextfield


type alias Model =
    { paymentDescription : String
    , amount : Maybe String
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
    { paymentDescription = ""
    , amount = Nothing
    , payor = Just session.user.id
    , payees = Set.empty
    , people = []
    , tags = ChipsTextfield.init
    , saveState = Composing
    }
        => Cmd.Extra.perform RefreshPeopleList


type Msg
    = RefreshPeopleList
    | RefreshPeopleListResponse (Result GraphQL.Client.Http.Error (List Person))
    | SelectPayor PersonId
    | TogglePayee PersonId
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
            model
                => cmd
                => Cmd.none

        RefreshPeopleListResponse result ->
            (case result of
                Err err ->
                    model

                Ok people ->
                    { model
                        | people = people

                        -- , payees = payeesForTransactionType model.transactionType session.user.id people
                    }
            )
                => (Dom.focus idsStr.paymentAmount |> Task.attempt ElementFocused)
                => Cmd.none

        SelectPayor personId ->
            let
                payees =
                    Set.empty

                -- payeesForTransactionType model.transactionType personId model.people
            in
            { model | payor = Just personId, payees = payees }
                => Cmd.none
                => Cmd.none

        TogglePayee personId ->
            { model | payees = Set.Extra.toggle personId model.payees }
                => Cmd.none
                => Cmd.none

        SetAmount str ->
            (if (String.trim >> String.length) str == 0 then
                { model | amount = Nothing }

             else
                case match moneyRegex str of
                    True ->
                        { model | amount = Just str }

                    False ->
                        model
            )
                => Cmd.none
                => Cmd.none

        SetPaymentDescription str ->
            { model | paymentDescription = str }
                => Cmd.none
                => Cmd.none

        ElementFocused res ->
            model
                => Cmd.none
                => Cmd.none

        OnPaymentAmountKeyDown keyCode ->
            let
                cmd =
                    if keyCode == 13 then
                        Dom.focus idsStr.paymentDescription |> Task.attempt ElementFocused

                    else
                        Cmd.none
            in
            model => cmd => Cmd.none

        OnPaymentDescriptionKeyDown keyCode ->
            let
                msg =
                    if keyCode == 13 then
                        [ Cmd.Extra.perform SaveTransaction ]

                    else
                        []
            in
            model
                => Cmd.none
                => Cmd.none

        TagsMsg msg ->
            let
                ( subModel, subMsg ) =
                    ChipsTextfield.update msg model.tags
            in
            { model | tags = subModel }
                => Cmd.map TagsMsg subMsg
                => Cmd.none

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
            { model | tags = newTags } => Cmd.none => Cmd.none

        SaveTransaction ->
            let
                newTransaction : Maybe NewTransaction
                newTransaction =
                    case
                        ( Maybe.withDefault "0" model.amount |> String.toInt
                        , Set.toList model.payees
                        , model.payor
                        )
                    of
                        ( Ok 0, _, _ ) ->
                            Nothing

                        ( Ok amount, payeeIds, Just payorId ) ->
                            { amount = amount * 100
                            , description = Just model.paymentDescription

                            -- , paidAt = Nothing
                            , payorId = payorId
                            , payeeIds = payeeIds
                            , tags = Just model.tags.chips
                            }
                                |> Just

                        _ ->
                            Nothing

                sendReqCmd =
                    newTransaction
                        |> Maybe.andThen (Just << addTransaction)
                        |> Maybe.andThen (Just << sendMutationRequest)
            in
            case sendReqCmd of
                Just cmd ->
                    { model | saveState = Saving }
                        => Task.attempt SaveTransactionResponse cmd
                        => Cmd.none

                _ ->
                    model
                        => Cmd.none
                        => Cmd.none

        SaveTransactionResponse result ->
            (case result of
                Err error ->
                    { model | saveState = SaveError }

                Ok res ->
                    { model | saveState = Saved (SaveResult res) }
            )
                => Cmd.none
                => Cmd.none

        OpenSavedTransaction ->
            case model.saveState of
                Saved { paymentId } ->
                    model
                        => Cmd.none
                        => Cmd.batch [ Route.modifyUrl <| Route.Transaction paymentId ]

                _ ->
                    model
                        => Cmd.none
                        => Cmd.none


{-| IDs useful for focusing visual elements.
-}
idsStr =
    { paymentAmount = "elm-payment-amount"
    , paymentDescription = "elm-payment-description"
    }


{-| IDs needed for Material UI to conveniently distinguish states of components.
-}
ids =
    { paymentDescription = 0
    , amount = 1
    , btnAdd = 2
    , catSel = 3
    , payorSelection = 4
    , payeeSelection = 5
    , tags = 6
    , usualTags = 7
    , btnOpenSavedPayment = 8
    }


isFormFilled : Model -> Bool
isFormFilled model =
    model.amount /= Nothing


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


view : Context msg -> Html msg
view ctx =
    let
        { model } =
            ctx

        formDisabled =
            isFormDisabled model
    in
    Opts.div [ css "width" "100%", css "user-select" "none" ]
        [ Opts.stylesheet newTransactionStylesheet
        , Opts.div
            [ Elevation.e2
            , Elevation.e0 |> Opts.when formDisabled
            , cs Window
            ]
            [ Opts.styled p [ cs Header ] [ text "New transaction" ]
            , Opts.div [ cs MainContainer ]
                [ Opts.div [ cs Main ]
                    [ viewAmount ctx
                    , viewMoneyDirection ctx
                    , Opts.div [] [ viewDescription ctx ]
                    , viewTags ctx
                    ]
                , viewUsualTags ctx
                ]
            , viewSave ctx
            , viewSaveResult ctx |> viewIf (model.saveState /= Composing)
            ]
        ]


viewAmount : Context msg -> Html msg
viewAmount ctx =
    let
        { model } =
            ctx
    in
    input
        [ Attr.type_ "number"
        , Attr.property "pattern" (Json.Encode.string "[0-9]*([,.][0-9]{2})?")
        , Attr.value <| Maybe.withDefault "" model.amount
        , Attr.id idsStr.paymentAmount
        , Attr.class <| cls Amount
        , Attr.placeholder "PLN"
        , Html.Events.onInput (ctx.lift << SetAmount)
        , Html.Events.on "keydown" (Json.map (ctx.lift << OnPaymentAmountKeyDown) Html.Events.keyCode)
        ]
        []



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


payorSelection : Context msg -> Html msg
payorSelection ctx =
    let
        { model } =
            ctx

        viewEl : Int -> Person -> Html msg
        viewEl idx person =
            let
                isSelected =
                    model.payor
                        |> Maybe.Extra.unwrap False (\pid -> pid == person.id)
            in
            Lists.li
                [ Opts.onClick (ctx.lift <| SelectPayor person.id) |> Opts.when (isFormEnabled model)
                , csg "interactive" |> Opts.cs |> Opts.when (isFormEnabled model)
                ]
                [ Lists.content []
                    [ Opts.span [ Lists.action2 ]
                        [ Toggles.radio
                            ctx.liftMaterial
                            [ ids.payorSelection, idx ]
                            ctx.mdl
                            [ Toggles.value isSelected
                            , Toggles.group "payorSelection"
                            , Opts.onToggle (ctx.lift <| SelectPayor person.id) |> Opts.when (isFormEnabled model)
                            , Toggles.ripple |> Opts.when (isFormEnabled model)
                            ]
                            []
                        ]
                    , text person.name
                    ]
                ]
    in
    div []
        [ ctx.model.people
            |> List.indexedMap viewEl
            |> Lists.ul []
        ]


payeeSelection : Context msg -> Html msg
payeeSelection ctx =
    let
        { model } =
            ctx

        viewEl : Int -> Person -> Html msg
        viewEl idx person =
            let
                isSelected =
                    model.payees |> Set.member person.id
            in
            Lists.li
                [ Opts.onClick (ctx.lift <| TogglePayee person.id) |> Opts.when (isFormEnabled model)
                , csg "interactive" |> Opts.cs |> Opts.when (isFormEnabled model)
                ]
                [ Lists.content []
                    [ Opts.span [ Lists.action2 ]
                        [ Toggles.checkbox
                            ctx.liftMaterial
                            [ ids.payeeSelection, idx ]
                            ctx.mdl
                            [ Toggles.value isSelected
                            , Opts.onToggle (ctx.lift <| TogglePayee person.id) |> Opts.when (isFormEnabled model)
                            , Toggles.ripple |> Opts.when (isFormEnabled model)
                            ]
                            []
                        ]
                    , text person.name
                    ]
                ]
    in
    div []
        [ ctx.model.people
            |> List.filter
                (\p ->
                    Just p.id /= model.payor
                )
            |> List.indexedMap viewEl
            |> Lists.ul []
        ]


viewMoneyDirection : Context msg -> Html msg
viewMoneyDirection ctx =
    let
        { model } =
            ctx
    in
    Opts.div [ cs MoneyDirection ]
        [ payorSelection ctx
        , Opts.span [ cs MoneyDirectionArrow ] [ text "⇢" ]
        , payeeSelection ctx
        ]


viewDescription : Context msg -> Html msg
viewDescription ctx =
    let
        { model } =
            ctx
    in
    Textfield.render ctx.liftMaterial
        [ ids.paymentDescription ]
        ctx.mdl
        [ Textfield.label
            (if isFormDisabled model then
                if String.isEmpty model.paymentDescription then
                    "No description"

                else
                    "Description"

             else
                "Description (optional)"
            )
        , Textfield.floatingLabel
        , Opts.onInput (ctx.lift << SetPaymentDescription)
        , Opts.on "keydown" (Json.map (ctx.lift << OnPaymentDescriptionKeyDown) Html.Events.keyCode)
        , Textfield.value model.paymentDescription
        , Opts.id idsStr.paymentDescription
        , cs Description
        ]
        []


viewTags : Context msg -> Html msg
viewTags ctx =
    let
        subContext =
            passContextWithoutSession ctx .tags (ctx.lift << TagsMsg) [ ids.tags ]

        text =
            if isFormDisabled ctx.model then
                "Tags (optional)"

            else
                "Tags"

        cfg =
            ChipsTextfield.Config (isFormEnabled ctx.model) text "No tags were selected."
    in
    Opts.span [ cs TagsContainer ]
        [ ChipsTextfield.view subContext cfg
        ]


viewUsualTags : Context msg -> Html msg
viewUsualTags ctx =
    let
        iconizedTags =
            [ ( "journey", "local_airport" )
            , ( "tool", "build" )
            , ( "car", "directions_car" )
            , ( "cash", "attach_money" )
            , ( "food", "restaurant" )
            ]
    in
    Opts.div [ cs UsualTags ] <|
        List.indexedMap (viewUsualTag ctx) iconizedTags


viewUsualTag : Context msg -> Int -> ( String, String ) -> Html msg
viewUsualTag ctx idx ( tagName, iconName ) =
    let
        selected =
            List.member tagName ctx.model.tags.chips
    in
    Opts.div [ cs UsualTagContainer ]
        [ Button.render ctx.liftMaterial
            [ ids.usualTags, idx ]
            ctx.mdl
            [ Button.fab
            , Button.minifab
            , Button.colored |> Opts.when selected
            , Button.ripple
            , cs UsualTag
            , Opts.onClick (ctx.lift <| ToggleTag tagName)
            ]
            [ Icon.i iconName ]
        ]


viewSave : Context msg -> Html msg
viewSave ctx =
    let
        {- TODO: transfer/shared payment/payment -}
        btnText =
            "Add transaction"
    in
    Opts.div [ cs BtnSaveContainer ]
        [ Button.render ctx.liftMaterial
            [ ids.btnAdd ]
            ctx.mdl
            [ Button.raised
            , Button.colored
            , Button.ripple
            , Button.disabled |> Opts.when ((not <| isFormFilled ctx.model) || isFormDisabled ctx.model)
            , Opts.onClick (ctx.lift SaveTransaction)
            ]
            [ text btnText ]
        ]


viewSaveResult : Context msg -> Html msg
viewSaveResult ctx =
    let
        { model } =
            ctx
    in
    Opts.div
        [ cs SaveInfo
        , cs WithMargin |> Opts.when (model.saveState /= Saving)
        ]
        [ case model.saveState of
            Composing ->
                text ""

            Saving ->
                Loading.indeterminate

            SaveError ->
                Opts.styled Html.p
                    [ cs SaveErrorText, Color.text (Color.color Color.Red Color.S600) ]
                    [ text "Some error occured, please try again or contact Administrator." ]

            Saved result ->
                Button.render ctx.liftMaterial
                    [ ids.btnOpenSavedPayment ]
                    ctx.mdl
                    [ Button.raised
                    , Button.colored
                    , Button.ripple
                    , Opts.onClick (ctx.lift OpenSavedTransaction)
                    ]
                    [ span [] [ text "Open Payment" ] ]
        ]
