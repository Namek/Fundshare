module PageDev._boilerplate exposing (Model, Msg, main, view)

import Browser
import Element
import Html exposing (Html)
import Page.<your_page> as ThePage
import PageDev.DevCommon as Dev exposing (Msg(..))


main : Program () Model Msg
main =
    Browser.element
        { init = always <| Dev.init ThePage.init
        , view = view
        , update = \msg model -> Dev.update ThePage.update handlePageMsg handleGlobalCmd msg model
        , subscriptions = always Sub.none
        }


type alias Context =
    ThePage.Context Msg


type alias Model =
    Dev.Model ThePage.Model


type alias Msg =
    Dev.Msg ThePage.Msg


handlePageMsg : Model -> ThePage.Msg -> Maybe ( Model, Cmd Msg )
handlePageMsg model pageMsg =
    Nothing


handleGlobalCmd model globalCmdMsg =
    Nothing


view : Model -> Html Msg
view model =
    Dev.view <|
        ThePage.view (Dev.makeCtx model)