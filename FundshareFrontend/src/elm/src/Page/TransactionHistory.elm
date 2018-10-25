module Page.TransactionHistory exposing (Model, Msg, init, update, view)

import Array exposing (Array)
import Cmd.Extra
import Data.Context exposing (ContextData, GlobalMsg, Logged)
import Data.Session exposing (Session)
import Data.Transaction exposing (Transaction)
import Element exposing (Element, column, paragraph, text)
import Misc exposing (noCmd)



-- MODEL --


type alias Model =
    { transactions : Pagination Transaction }


init : Session -> ( Model, Cmd Msg )
init session =
    ( { transactions =
            { elements = Array.empty
            , resultsPerPage = 20
            , lastRequest = Nothing
            }
      }
    , Cmd.Extra.perform <| LoadPage 1
    )


type alias Context msg =
    Logged (ContextData Model Msg msg)


{-| Pagination with cached elements
-}
type alias Pagination data =
    { elements : Array data
    , resultsPerPage : Int
    , lastRequest : Maybe PaginationRequest
    }


type alias PaginationRequest =
    { offset : Int
    , limit : Int
    }


getCurrentPageIndex : Pagination data -> Int
getCurrentPageIndex pagination =
    case pagination.lastRequest of
        Just req ->
            req.offset // pagination.resultsPerPage

        Nothing ->
            0


createNextPageRequest pagination =
    let
        pageIndex =
            getCurrentPageIndex pagination

        newOffset =
            (pageIndex + 1) * pagination.resultsPerPage
    in
    { offset = newOffset, limit = pagination.resultsPerPage }



-- UPDATE --


type Msg
    = LoadPage Int


update : Context msg -> Msg -> ( ( Model, Cmd Msg ), Cmd GlobalMsg )
update { model } msg =
    case msg of
        LoadPage pageNo ->
            model |> noCmd |> noCmd



-- VIEW --


view : Context msg -> Element msg
view { model } =
    paragraph [] [ text <| "History" ]


viewHistoryTransactions : Context msg -> Element msg
viewHistoryTransactions ctx =
    column [] []
