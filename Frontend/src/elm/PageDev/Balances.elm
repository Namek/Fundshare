module PageDev.Balances exposing (Context, Model, Msg, handleGlobalCmd, handlePageMsg, main, view)

import Browser
import Cmd.Extra
import Element
import Html exposing (Html)
import Page.Balances as ThePage exposing (Msg(..))
import PageDev.Data.Balances as Balances
import PageDev.DevCommon as Dev exposing (Msg(..))
import Process
import RemoteData
import Task


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
    case pageMsg of
        RefreshBalances ->
            let
                answerWithBalances =
                    Process.sleep 1000
                        |> Task.perform
                            (always <|
                                GotPageMsg <|
                                    RefreshBalances_Response (RemoteData.succeed Balances.balances)
                            )
            in
            Just ( model, Cmd.batch [ answerWithBalances ] )

        _ ->
            Nothing


handleGlobalCmd model globalCmdMsg =
    Nothing


view : Model -> Html Msg
view model =
    Dev.view <|
        ThePage.view (Dev.makeCtx model)
