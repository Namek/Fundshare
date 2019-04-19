module Misc.Pagination exposing
    ( Pagination
    , PaginationRequest
    , createNextPageRequest
    , createPageRequest
    , currentOffset
    , currentPageIndex
    , currentPageNo
    , furthestPageIndex
    , getMultiple
    , hasNoMorePages
    , isPageLoaded
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


currentPageNo : Pagination data -> Int
currentPageNo pagination =
    currentPageIndex pagination + 1


furthestPageIndex : Pagination data -> Int
furthestPageIndex pagination =
    Dict.keys pagination.elements
        |> List.maximum
        |> Maybe.withDefault 0
        |> (\idx -> idx // pagination.resultsPerPage)


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
    inner (max 0 <| limit - 1) []


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


isPageLoaded : Int -> Pagination data -> Bool
isPageLoaded pageNo pagination =
    let
        limit =
            pagination.resultsPerPage

        startIndex =
            (pageNo - 1) * limit
    in
    getMultiple pagination.elements startIndex limit
        |> List.member Nothing
        |> not


viewPaginator : Pagination data -> Element msg
viewPaginator pagination =
    let
        pageIndex =
            currentPageIndex pagination

        nextPageIndex =
            pageIndex + 1

        maxPageIndex =
            furthestPageIndex pagination

        previousNumbers =
            if pageIndex > 0 then
                List.range 1 (min pageIndex maxPageIndex)

            else
                []

        nextNumbers =
            if pageIndex == maxPageIndex then
                []

            else
                List.range (pageIndex + 2) (maxPageIndex + 1)

        canShowNextPage =
            (pageIndex < maxPageIndex)
                || (not <| hasNoMorePages pagination)

        viewNumberLink number =
            link []
                { url = Route.routeToString (Route.TransactionHistory (Just number))
                , label = number |> String.fromInt |> text
                }
    in
    row [ spacing 15, Font.size 14 ]
        [ link []
            { label = text "prev page"
            , url = Route.routeToString (Route.TransactionHistory (Just <| nextPageIndex - 1))
            }
            |> viewIf (pageIndex > 0)
        , row [ spacing 5 ]
            (previousNumbers |> List.map viewNumberLink)
        , text ((pageIndex + 1) |> String.fromInt)
        , row [ spacing 5 ]
            (nextNumbers |> List.map viewNumberLink)
        , link []
            { label = text "next page"
            , url = Route.routeToString (Route.TransactionHistory (Just <| nextPageIndex + 1))
            }
            |> viewIf canShowNextPage
        ]
