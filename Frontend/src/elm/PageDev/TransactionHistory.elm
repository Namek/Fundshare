module PageDev.TransactionHistory exposing (Model, Msg, main, view)

import Browser
import Cmd.Extra
import Html exposing (Html)
import Page.TransactionHistory as ThePage exposing (Msg(..))
import PageDev.Data.Transactions as Transactions
import PageDev.DevCommon as Dev exposing (Msg(..))
import Process
import RemoteData
import Request.Transactions exposing (TransactionList)
import Task


main : Program () Model Msg
main =
    Browser.element
        { init = always <| Dev.init (ThePage.init Nothing)
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
        LoadPage pageNo ->
            let
                perPage =
                    2

                offset =
                    (pageNo - 1) * perPage

                transactions =
                    Transactions.transactions
                        -- reverse for date DESC order
                        |> List.reverse
                        |> List.drop offset
                        |> List.take perPage

                pageData : TransactionList
                pageData =
                    { transactions = transactions
                    , offset = offset
                    , limit = perPage
                    }

                answerWithPageData =
                    Process.sleep 1000
                        |> Task.perform
                            (always <|
                                GotPageMsg <|
                                    LoadPage_Response (RemoteData.succeed pageData)
                            )
            in
            Just ( model, answerWithPageData )

        _ ->
            Nothing


handleGlobalCmd model globalCmdMsg =
    Nothing


view : Model -> Html Msg
view model =
    Dev.view <|
        ThePage.view (Dev.makeCtx model)
