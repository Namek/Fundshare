module Page.TransactionHistory exposing (Model, Msg, init, reinit, update, view)

import Cmd.Extra
import Data.Context exposing (ContextData, GlobalMsg, Logged, subContext)
import Data.Person exposing (Person)
import Data.Session exposing (Session)
import Data.Transaction exposing (Transaction)
import Dict exposing (Dict)
import Element exposing (Element, centerX, column, fill, link, paragraph, px, row, spacing, text, width)
import Element.Font as Font
import Graphql.Http
import Html exposing (i)
import Html.Attributes
import List exposing (range)
import List.Extra
import Misc exposing (attr, either, getUpdatedProperty, noCmd, styledButton, viewIf, viewLoadingBar)
import Misc.Pagination as Pagination exposing (Pagination, hasNoMorePages)
import RemoteData exposing (RemoteData)
import Request.Common exposing (sendQueryRequest)
import Request.Transactions exposing (TransactionList, getUserTransactions)
import Route
import Views.Timeline as Timeline



-- MODEL --


type alias Model =
    { transactions : Pagination Transaction
    , isPageLoading : Bool
    , timeline : Timeline.Model
    }


init : Maybe Int -> Session -> ( Model, Cmd Msg )
init pageNo session =
    let
        ( timeline, timelineMsg ) =
            Timeline.init [] []
    in
    ( { transactions =
            { elements = Dict.empty
            , lastRequest = Nothing
            , resultsPerPage = 60
            }
      , isPageLoading = True
      , timeline = timeline
      }
    , Cmd.batch
        [ Cmd.map Timeline_Msg timelineMsg
        , Cmd.Extra.perform <| LoadPage (pageNo |> Maybe.withDefault 1)
        ]
    )


reinit : Model -> Maybe Int -> Session -> ( Model, Cmd Msg )
reinit model pageNo session =
    ( model, Cmd.Extra.perform <| LoadPage (pageNo |> Maybe.withDefault 1) )


type alias Context msg =
    Logged (ContextData Model Msg msg)



-- UPDATE --


type Msg
    = LoadPage Int
    | LoadNextPage
    | LoadPage_Response (RemoteData (Graphql.Http.Error TransactionList) TransactionList)
    | Timeline_Msg Timeline.Msg


update : Context msg -> Msg -> ( ( Model, Cmd Msg ), Cmd GlobalMsg )
update { model } msg =
    case msg of
        LoadPage pageNo ->
            let
                pageRequest =
                    Pagination.createPageRequest model.transactions pageNo

                updatedTransactions =
                    model |> getUpdatedProperty .transactions (\t -> { t | lastRequest = Just pageRequest })

                cmd =
                    if Pagination.isPageLoaded pageNo model.transactions then
                        Cmd.none

                    else
                        getUserTransactions pageRequest.offset pageRequest.limit
                            |> sendQueryRequest LoadPage_Response
            in
            ( { model | transactions = updatedTransactions }, cmd ) |> noCmd

        LoadNextPage ->
            let
                cmd =
                    LoadPage <| Pagination.currentPageNo model.transactions + 1
            in
            ( model, Cmd.Extra.perform cmd ) |> noCmd

        LoadPage_Response RemoteData.Loading ->
            { model | isPageLoading = True } |> noCmd |> noCmd

        LoadPage_Response (RemoteData.Success transactionList) ->
            let
                { offset, limit, transactions } =
                    transactionList

                modelTransactions =
                    model.transactions

                updatedElements =
                    Pagination.setMultiple modelTransactions.elements offset transactions

                updatedModelTransactions =
                    { modelTransactions | elements = updatedElements }
            in
            { model
                | isPageLoading = False
                , transactions = updatedModelTransactions
                , timeline = Timeline.insertTransactionsToModel model.timeline transactions
            }
                |> noCmd
                |> noCmd

        LoadPage_Response _ ->
            { model | isPageLoading = False } |> noCmd |> noCmd

        Timeline_Msg timelineMsg ->
            let
                ( subModel, subMsg ) =
                    Timeline.update model.timeline timelineMsg
            in
            ( { model | timeline = subModel }, Cmd.map Timeline_Msg subMsg ) |> noCmd



-- VIEW --


view : Context msg -> Element msg
view ctx =
    let
        subCtx =
            subContext ctx .timeline Timeline_Msg

        isAnythingLoaded =
            Dict.size ctx.model.transactions.elements > 0
    in
    if isAnythingLoaded then
        Element.column [ spacing 15 ]
            [ Timeline.view subCtx
            , styledButton [ centerX ]
                { onPress = Just <| ctx.lift LoadNextPage
                , label = text "Load more..."
                }
            ]

    else
        viewLoadingBar
