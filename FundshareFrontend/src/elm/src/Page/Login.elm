module Page.Login exposing (Model, Msg, initialModel, update, view)

import Cmd.Extra
import Data.Context exposing (ContextData, GlobalMsg(..))
import Data.Session exposing (Session, SessionState(..))
import GraphQL.Client.Http
import Html exposing (Html, div, p, text)
import Html.Events
import Json.Decode as Json
import Material.Button as Button
import Material.Elevation as Elevation
import Material.Options as Options exposing (css, when)
import Material.Progress as Loading
import Material.Textfield as Textfield
import Material.Typography as Typo
import Misc exposing ((=>), emailRegex, match, viewIf)
import Request.Common exposing (..)
import Request.Session exposing (..)
import Route
import Styles.Login exposing (LoginStyleClass(..), cs, loginStylesheet)
import Task


type alias Model =
    { email : String
    , password : String
    , focusedField : Field
    , isLoading : Bool
    }


initialModel : Model
initialModel =
    { email = ""
    , password = ""
    , focusedField = None
    , isLoading = False
    }


type Msg
    = SetEmail String
    | SetPassword String
    | OnFieldFocused Field
    | OnFieldBlurred
    | OnInFormKeyDown Int
    | SignIn
    | SignInResponse (Result GraphQL.Client.Http.Error SignInResult)


type alias Context msg =
    ContextData Model Msg msg


update : Context msg -> Msg -> ( ( Model, Cmd Msg ), Cmd GlobalMsg )
update ctx msg =
    let
        { model } =
            ctx
    in
    case msg of
        SetEmail email ->
            { model | email = email } => Cmd.none => Cmd.none

        SetPassword password ->
            { model | password = password } => Cmd.none => Cmd.none

        OnFieldFocused field ->
            { model | focusedField = field } => Cmd.none => Cmd.none

        OnFieldBlurred ->
            { model | focusedField = None } => Cmd.none => Cmd.none

        OnInFormKeyDown keyCode ->
            let
                msg =
                    if keyCode == 13 && isLoginFilled model then
                        Cmd.Extra.perform SignIn

                    else
                        Cmd.none
            in
            model => msg => Cmd.none

        SignIn ->
            let
                cmd =
                    signIn model
                        |> sendMutationRequest
                        |> Task.attempt SignInResponse
            in
            { model | isLoading = True }
                => cmd
                => Cmd.none

        SignInResponse res ->
            case res of
                Err error ->
                    { model | isLoading = False } => Cmd.none => Cmd.none

                Ok user ->
                    model
                        => Cmd.batch
                            [ Route.modifyUrl Route.Balances ]
                        => Cmd.batch
                            [ SetSession (Just { user = user }) |> Cmd.Extra.perform ]


ids =
    { username = 0
    , password = 1
    , loginButton = 2
    }


view : Context msg -> Html msg
view ctx =
    let
        { lift, liftMaterial, mdl, model } =
            ctx

        isPasswordTooShort =
            String.length model.password < minPasswordLength

        shouldErrorEmail =
            if String.isEmpty model.email || model.focusedField == Email then
                False

            else
                not <| match emailRegex model.email

        shouldErrorPassword =
            if String.isEmpty model.password || model.focusedField == Password then
                False

            else
                isPasswordTooShort

        formDisabled =
            model.isLoading
    in
    Options.div
        [ Elevation.e4
        , cs Form
        ]
        [ Options.stylesheet loginStylesheet
        , Options.styled p
            [ Typo.display2 ]
            [ text "Hi." ]
        , div []
            [ Textfield.render liftMaterial
                (ctx.matId [ ids.username ])
                mdl
                [ Textfield.label "E-mail"
                , Textfield.floatingLabel
                , Textfield.email
                , Options.onFocus (lift <| OnFieldFocused Email)
                , Options.onBlur (lift <| OnFieldBlurred)
                , Options.onInput (lift << SetEmail)
                , Options.on "keydown" (Json.map (ctx.lift << OnInFormKeyDown) Html.Events.keyCode)
                , Textfield.disabled |> when formDisabled
                , Textfield.value model.email
                , Textfield.error "Is not e-mail"
                    |> when shouldErrorEmail
                ]
                []
            ]
        , div []
            [ Textfield.render liftMaterial
                (ctx.matId [ ids.password ])
                mdl
                [ Textfield.label "Password"
                , Textfield.floatingLabel
                , Textfield.password
                , Options.onFocus (lift <| OnFieldFocused Password)
                , Options.onBlur (lift <| OnFieldBlurred)
                , Options.onInput (lift << SetPassword)
                , Options.on "keydown" (Json.map (ctx.lift << OnInFormKeyDown) Html.Events.keyCode)
                , Textfield.disabled |> when formDisabled
                , Textfield.value model.password
                , Textfield.error ("Minimum " ++ String.fromInt minPasswordLength ++ " characters")
                    |> when shouldErrorPassword
                ]
                []
            ]
        , Button.render liftMaterial
            (ctx.matId [ ids.loginButton ])
            mdl
            [ cs BtnLogin
            , Button.disabled |> when ((not <| isLoginFilled model) || formDisabled)
            , Button.raised
            , Button.colored |> when (isLoginFilled model)
            , Button.ripple
            , Options.onClick (lift <| SignIn)
            ]
            [ text "Sign In" ]
        , viewIf model.isLoading Loading.indeterminate
        ]



-- VALIDATION --


minPasswordLength : number
minPasswordLength =
    7


isLoginFilled : Model -> Bool
isLoginFilled model =
    match emailRegex model.email
        && (String.length model.password >= minPasswordLength)


type Field
    = None
    | Email
    | Password
