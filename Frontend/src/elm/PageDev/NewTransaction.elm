module PageDev.NewTransaction exposing (Model, Msg, main, view)

import Browser
import Cmd.Extra
import Html exposing (Html)
import Page.NewTransaction as ThePage exposing (Msg(..))
import PageDev.DevCommon as Dev exposing (Msg(..))
import RemoteData


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
        SaveTransaction edit ->
            let
                transactionId =
                    1

                answerWithTransactionList =
                    (Cmd.Extra.perform << GotPageMsg) <|
                        SaveTransaction_Response (RemoteData.succeed transactionId)
            in
            Just ( model, Cmd.batch [ answerWithTransactionList ] )

        _ ->
            Nothing


handleGlobalCmd model globalCmdMsg =
    Nothing


view : Model -> Html Msg
view model =
    Dev.view <|
        ThePage.view (Dev.makeCtx model)
