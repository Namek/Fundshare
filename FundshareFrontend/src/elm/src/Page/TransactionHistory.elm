module Page.TransactionHistory exposing (Model, Msg, init, reinit, update, view)

import Cmd.Extra
import Data.Context exposing (ContextData, GlobalMsg, Logged)
import Data.Session exposing (Session)
import Data.Transaction exposing (Transaction)
import Dict exposing (Dict)
import Element exposing (Element, column, link, paragraph, row, spacing, text)
import Element.Font as Font
import Graphql.Http
import Html exposing (i)
import List exposing (range)
import List.Extra
import Misc exposing (getUpdatedProperty, noCmd, viewIf)
import Misc.Pagination as Pagination exposing (Pagination, hasNoMorePages)
import RemoteData exposing (RemoteData)
import Request.Common exposing (sendQueryRequest)
import Request.Transactions exposing (TransactionList, getUserTransactions)
import Route



-- MODEL --


type alias Model =
    { transactions : Pagination Transaction
    , isPageLoading : Bool
    }


init : Maybe Int -> Session -> ( Model, Cmd Msg )
init pageNo session =
    ( { transactions =
            { elements = Dict.empty
            , lastRequest = Nothing
            , resultsPerPage = 20
            }
      , isPageLoading = True
      }
    , Cmd.Extra.perform <|
        LoadPage (pageNo |> Maybe.withDefault 1)
    )


reinit : Model -> Maybe Int -> Session -> ( Model, Cmd Msg )
reinit model pageNo session =
    ( model, Cmd.Extra.perform <| LoadPage (pageNo |> Maybe.withDefault 1) )


type alias Context msg =
    Logged (ContextData Model Msg msg)



-- UPDATE --


type Msg
    = LoadPage Int
    | LoadPage_Response (RemoteData (Graphql.Http.Error TransactionList) TransactionList)


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
            { model | isPageLoading = False, transactions = updatedModelTransactions }
                |> noCmd
                |> noCmd

        LoadPage_Response _ ->
            { model | isPageLoading = False } |> noCmd |> noCmd



-- VIEW --


view : Context msg -> Element msg
view ctx =
    column []
        [ Pagination.viewPaginator ctx.model.transactions
        , viewHistoryTransactions ctx
        ]


viewHistoryTransactions : Context msg -> Element msg
viewHistoryTransactions ctx =
    column []
        (ctx.model.transactions
            |> Pagination.mapElements (\t -> paragraph [] [ text <| String.fromInt <| t.id ])
        )
