module Misc.Pagination exposing
    ( Pagination
    , PaginationRequest
    , createNextPageRequest
    , createPageRequest
    , currentOffset
    , currentPageIndex
    , furthestPageIndex
    , getMultiple
    , hasNoMorePages
    , mapElements
    , setMultiple
    , viewPaginator
    )

import Dict exposing (Dict)
import Element exposing (Element, link, row, spacing, text)
import Element.Font as Font
import Misc exposing (viewIf)
import Route


{-| Pagination with cached elements
-}
type alias Pagination data =
    { elements : Dict Int data
    , resultsPerPage : Int
    , lastRequest : Maybe PaginationRequest
    }


type alias PaginationRequest =
    { offset : Int
    , limit : Int
    }


currentOffset : Pagination data -> Int
currentOffset pagination =
    case pagination.lastRequest of
        Just req ->
            req.offset

        Nothing ->
            0


currentPageIndex : Pagination data -> Int
currentPageIndex pagination =
    let
        limit =
            pagination.resultsPerPage
    in
    currentOffset pagination // limit


furthestPageIndex : Pagination data -> Int
furthestPageIndex pagination =
    let
        iter pageIndex elIndex =
            if Dict.member elIndex pagination.elements then
                iter (pageIndex + 1) (elIndex + pagination.resultsPerPage)

            else
                pageIndex
    in
    iter 0 0


createPageRequest pagination pageNo =
    let
        limit =
            pagination.resultsPerPage

        newOffset =
            (pageNo - 1) * limit
    in
    { offset = newOffset, limit = limit }


createNextPageRequest pagination =
    let
        pageIndex =
            currentPageIndex pagination

        limit =
            pagination.resultsPerPage

        newOffset =
            pageIndex * limit
    in
    { offset = newOffset, limit = limit }


hasNoMorePages pagination =
    modBy pagination.resultsPerPage (Dict.size pagination.elements) /= 0


setMultiple : Dict Int data -> Int -> List data -> Dict Int data
setMultiple dict startIndex elements =
    case elements of
        [] ->
            dict

        x :: xs ->
            let
                newDict =
                    Dict.insert startIndex x dict
            in
            setMultiple newDict (startIndex + 1) xs


getMultiple : Dict Int data -> Int -> Int -> List (Maybe data)
getMultiple dict startIndex limit =
    let
        inner leftLimit accList =
            case leftLimit of
                0 ->
                    accList

                offset ->
                    let
                        el =
                            Dict.get (startIndex + offset) dict
                    in
                    inner (offset - 1) (el :: accList)
    in
    inner limit []


mapElements : (data -> mapped) -> Pagination data -> List mapped
mapElements mapper pagination =
    let
        offset =
            currentOffset pagination
    in
    getMultiple pagination.elements offset pagination.resultsPerPage
        |> List.filterMap
            (\maybeEl ->
                case maybeEl of
                    Just el ->
                        Just <| mapper el

                    Nothing ->
                        Nothing
            )


viewPaginator : Pagination data -> Element msg
viewPaginator pagination =
    let
        pageIndex =
            currentPageIndex pagination

        nextPageIndex =
            pageIndex + 1

        maxPageIndex =
            max pageIndex (furthestPageIndex pagination)

        previousNumbers =
            if pageIndex > 0 then
                List.range 1 maxPageIndex

            else
                []
    in
    row [ spacing 15, Font.size 14 ]
        [ link []
            { label = text "prev page"
            , url = Route.routeToString (Route.TransactionHistory (Just <| nextPageIndex - 1))
            }
            |> viewIf (pageIndex > 0)
        , row [ spacing 5 ]
            (previousNumbers
                |> List.map
                    (\number ->
                        link []
                            { url = Route.routeToString (Route.TransactionHistory (Just number))
                            , label = number |> String.fromInt |> text
                            }
                    )
            )
        , text ((pageIndex + 1) |> String.fromInt)
        , link []
            { label = text "next page"
            , url = Route.routeToString (Route.TransactionHistory (Just <| nextPageIndex + 1))
            }
            |> viewIf (not <| hasNoMorePages pagination)
        ]
