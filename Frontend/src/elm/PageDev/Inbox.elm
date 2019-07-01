module PageDev.Inbox exposing (Model, Msg, main, view)

import Browser
import Cmd.Extra
import Data.Transaction exposing (Transaction)
import Html exposing (Html)
import List.Extra
import Page.Inbox as ThePage exposing (Msg(..), MsgCards(..))
import PageDev.Data.Transactions as Transactions
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
        RefreshTransactions ->
            let
                answerWithTransactionList =
                    (Cmd.Extra.perform << GotPageMsg) <|
                        RefreshTransactions_Response (RemoteData.succeed Transactions.transactionList)
            in
            Just ( model, Cmd.batch [ answerWithTransactionList ] )

        MsgCards (SaveTransaction id data) ->
            Transactions.transactions
                |> List.Extra.find (\t -> t.id == id)
                |> Maybe.map
                    (\transaction ->
                        let
                            -- simulate a change in database
                            editedTransaction : Transaction
                            editedTransaction =
                                { transaction
                                    | amount = data.amount
                                    , payorId = data.payorId
                                    , description = data.description
                                    , beneficientIds = data.beneficientIds
                                    , tags = data.tags
                                    , acceptanceIds = [ model.session.id ]
                                }

                            answerWithEditedTransaction =
                                -- simulate some time delay to let the animation show off
                                Process.sleep 1000
                                    |> Task.perform
                                        (always <|
                                            (GotPageMsg << MsgCards) <|
                                                SaveTransaction_Response (RemoteData.succeed editedTransaction)
                                        )

                            -- let the SaveTransaction cmd itself update the subModel for saving animation
                            ( ( newSubModel, localCmds ), globalCmds ) =
                                ThePage.update (Dev.makeCtx model) pageMsg
                        in
                        Just ( { model | subModel = newSubModel }, Cmd.batch [ answerWithEditedTransaction ] )
                    )
                |> Maybe.withDefault Nothing

        MsgCards (AcceptTransaction id) ->
            let
                answerWithAcceptedTransactions =
                    -- simulate some time delay to let the animation show off
                    Process.sleep 1000
                        |> Task.perform
                            (always <|
                                (GotPageMsg << MsgCards) <|
                                    AcceptTransaction_Response (RemoteData.succeed { acceptedIds = [ id ], failedIds = [] })
                            )
            in
            Just ( model, Cmd.batch [ answerWithAcceptedTransactions ] )

        _ ->
            Nothing


handleGlobalCmd model globalCmdMsg =
    Nothing


view : Model -> Html Msg
view model =
    Dev.view <|
        ThePage.view (Dev.makeCtx model)
