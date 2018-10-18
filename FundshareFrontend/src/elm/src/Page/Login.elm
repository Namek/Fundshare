module Page.Login exposing (Model, Msg, initialModel, update, view)

import Cmd.Extra
import Data.Context exposing (ContextData, GlobalMsg(..))
import Data.Session exposing (Session, SessionState(..))
import Element exposing (Element, column, padding, paragraph, row, text)
import Element.Font as Font
import Element.Input as Input
import Graphql.Http as Http
import Html.Events
import Json.Decode as Json
import Misc exposing (emailRegex, match, viewIf)
import RemoteData exposing (RemoteData)
import Request.Common exposing (..)
import Request.Session exposing (..)
import Route
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
    | SignIn_Response (RemoteData (Http.Error SignInResult) SignInResult)


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
            ( ( { model | email = email }, Cmd.none ), Cmd.none )

        SetPassword password ->
            ( ( { model | password = password }, Cmd.none ), Cmd.none )

        OnFieldFocused field ->
            ( ( { model | focusedField = field }, Cmd.none ), Cmd.none )

        OnFieldBlurred ->
            ( ( { model | focusedField = None }, Cmd.none ), Cmd.none )

        OnInFormKeyDown keyCode ->
            let
                cmd =
                    if keyCode == 13 && isLoginFilled model then
                        Cmd.Extra.perform SignIn

                    else
                        Cmd.none
            in
            ( ( model, cmd ), Cmd.none )

        SignIn ->
            let
                cmd =
                    signIn model
                        |> sendMutationRequest SignIn_Response
            in
            ( ( { model | isLoading = True }, cmd ), Cmd.none )

        SignIn_Response res ->
            case res of
                RemoteData.Success user ->
                    ( ( model, Cmd.none )
                    , Cmd.batch <|
                        List.map Cmd.Extra.perform
                            [ SetSession (Just { user = user })
                            , Navigate Route.Balances
                            ]
                    )

                _ ->
                    ( ( { model | isLoading = False }, Cmd.none ), Cmd.none )


ids =
    { username = 0
    , password = 1
    , loginButton = 2
    }


view : Context msg -> Element msg
view ctx =
    let
        { lift, model } =
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
    column
        [ padding 25
        , Element.inFront <|
            viewIf model.isLoading (paragraph [] [ text "Signing in..." ])
        ]
        [ {- Options.stylesheet loginStylesheet
             ,
          -}
          paragraph
            [ Font.size 18 ]
            [ text "Hi." ]
        , column []
            [ Input.email
                []
                { onChange = lift << SetEmail
                , text = model.email
                , placeholder = Nothing
                , label = Input.labelAbove [] (Element.text "E-mail")
                }

            -- , Options.onFocus (lift <| OnFieldFocused Email)
            -- , Options.onBlur (lift <| OnFieldBlurred)
            -- , Options.on "keydown" (Json.map (ctx.lift << OnInFormKeyDown) Html.Events.keyCode)
            -- , Textfield.disabled |> when formDisabled
            -- , Textfield.error "Is not e-mail"   |> when shouldErrorEmail
            ]
        , row []
            [ Input.currentPassword
                [ --  Options.onFocus (lift <| OnFieldFocused Password)
                  -- , Options.onBlur (lift <| OnFieldBlurred)
                  -- , Options.on "keydown" (Json.map (ctx.lift << OnInFormKeyDown) Html.Events.keyCode)
                  -- , Textfield.disabled |> when formDisabled
                  -- , Textfield.error ("Minimum " ++ String.fromInt minPasswordLength ++ " characters")  |> when shouldErrorPassword
                  Element.htmlAttribute <| Html.Events.on "keydown" (Json.map (ctx.lift << OnInFormKeyDown) Html.Events.keyCode)
                ]
                { onChange = lift << SetPassword
                , text = model.password
                , placeholder = Nothing
                , label = Input.labelAbove [] (text "Password")
                , show = False
                }
            ]
        , Input.button
            [--Button.disabled |> when ((not <| isLoginFilled model) || formDisabled)
             --, Button.raised
             --, Button.colored |> when (isLoginFilled model)
            ]
            { onPress = Just (lift <| SignIn)
            , label = text "Sign In"
            }
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
