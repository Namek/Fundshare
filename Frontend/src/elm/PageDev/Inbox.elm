module PageDev.Inbox exposing (Model, Msg, main, view)

import Browser
import Cmd.Extra
import Html exposing (Html)
import Page.Inbox as ThePage exposing (Msg(..))
import PageDev.Data.Transactions as Transactions
import PageDev.DevCommon as Dev exposing (Msg(..))
import RemoteData


main : Program () Model Msg
main =
    Browser.element
        { init = always <| Dev.init ThePage.init
        , view = view
        , update = \msg model -> Dev.update ThePage.update handlePageCmd handleGlobalCmd msg model
        , subscriptions = always Sub.none
        }


type alias Context =
    ThePage.Context Msg


type alias Model =
    Dev.Model ThePage.Model


type alias Msg =
    Dev.Msg ThePage.Msg


handlePageCmd model pageCmdMsg =
    case pageCmdMsg of
        RefreshTransactions ->
            let
                answerWithTransactionList =
                    Cmd.Extra.perform <|
                        GotPageMsg <|
                            RefreshTransactions_Response (RemoteData.succeed Transactions.transactionList)
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
