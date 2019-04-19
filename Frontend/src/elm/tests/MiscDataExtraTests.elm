module MiscDataExtraTests exposing (suite)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Misc.DataExtra exposing (updateOrAddOrdered)
import Test exposing (..)


type alias TestRecord =
    { value : Int }


type alias TestRecord2 =
    { key : Int, value : Int }


updateOrAddOrdered_testCase_numbers ( description, ( list, newEl, expectedList ) ) =
    test description <|
        \_ ->
            updateOrAddOrdered (compare newEl) identity (\() -> newEl) list
                |> Expect.equalLists expectedList


updateOrAddOrdered_testCase_recordsInserting ( description, ( list, newEl, expectedList ) ) =
    let
        compareEl =
            \el -> compare newEl.value el.value

        getEmptyEl =
            \() -> { value = 0 }

        updateEl el =
            { el | value = newEl.value }
    in
    test description <|
        \_ ->
            updateOrAddOrdered compareEl updateEl getEmptyEl list
                |> Expect.equalLists expectedList


updateOrAddOrdered_testCase_recordsUpdating ( description, ( list, newEl, expectedList ) ) =
    test description <|
        \_ ->
            updateOrAddOrdered_records newEl list
                |> Expect.equalLists expectedList


updateOrAddOrdered_records newEl list =
    let
        compareEl =
            \el -> compare newEl.key el.key

        getEmptyEl =
            \() -> { newEl | value = 0 }

        updateEl el =
            { el | value = newEl.value }
    in
    updateOrAddOrdered compareEl updateEl getEmptyEl list


numsToRecords nums =
    nums |> List.map (\num -> { value = num })


numsToRecords2 nums =
    nums |> List.map (\num -> { key = num, value = 0 })


keyed key =
    kv key 0


kv key value =
    { key = key, value = value }


updateOrAddOrdered_testCases =
    [ describe "inserting numbers" <|
        List.map updateOrAddOrdered_testCase_numbers
            [ ( "insert in the middle", ( [ 4, 6, 7 ], 5, [ 4, 5, 6, 7 ] ) )
            , ( "insert at the beginning", ( [ 4, 6, 7 ], 3, [ 3, 4, 6, 7 ] ) )
            , ( "insert at the end", ( [ 4, 6, 7 ], 8, [ 4, 6, 7, 8 ] ) )
            , ( "insert before end", ( [ 4, 6, 7, 9 ], 8, [ 4, 6, 7, 8, 9 ] ) )
            ]
    , describe "inserting records" <|
        List.map updateOrAddOrdered_testCase_recordsInserting
            [ ( "insert at the beginning", ( numsToRecords [ 4, 6 ], { value = 3 }, numsToRecords [ 3, 4, 6 ] ) )
            , ( "insert in the middle", ( numsToRecords [ 4, 6 ], { value = 5 }, numsToRecords [ 4, 5, 6 ] ) )
            , ( "insert at the end", ( numsToRecords [ 4, 6, 7 ], { value = 8 }, numsToRecords [ 4, 6, 7, 8 ] ) )
            , ( "insert before end", ( numsToRecords [ 4, 6, 7, 9 ], { value = 8 }, numsToRecords [ 4, 6, 7, 8, 9 ] ) )
            ]
    , describe "updating records" <|
        List.map updateOrAddOrdered_testCase_recordsUpdating
            [ ( "update first element", ( [ kv 4 0, kv 5 0, kv 6 0 ], kv 4 1, [ kv 4 1, kv 5 0, kv 6 0 ] ) )
            , ( "update last element", ( [ kv 4 0, kv 5 0, kv 6 0 ], kv 6 1, [ kv 4 0, kv 5 0, kv 6 1 ] ) )
            , ( "update middle element", ( [ kv 4 0, kv 5 0, kv 6 0 ], kv 5 1, [ kv 4 0, kv 5 1, kv 6 0 ] ) )
            ]
    , test "insert records sequentially" <|
        \_ ->
            [ kv 4 0 ]
                |> updateOrAddOrdered_records (kv 4 1)
                |> updateOrAddOrdered_records (kv 3 0)
                |> updateOrAddOrdered_records (kv 3 1)
                |> updateOrAddOrdered_records (kv 2 0)
                |> updateOrAddOrdered_records (kv 6 0)
                |> updateOrAddOrdered_records (kv 5 0)
                |> updateOrAddOrdered_records (kv 5 1)
                |> updateOrAddOrdered_records (kv 4 0)
                |> Expect.equalLists [ kv 2 0, kv 3 1, kv 4 0, kv 5 1, kv 6 0 ]
    ]


suite : Test
suite =
    describe "Misc.DataExtra"
        [ describe "updateOrAddOrdered" <|
            updateOrAddOrdered_testCases
        ]
