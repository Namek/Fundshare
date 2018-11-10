module Misc.DataExtra exposing (toggle, updateOrAddOrdered)

import Set exposing (Set)


updateOrAddOrdered : (el -> Order) -> (el -> el) -> (() -> el) -> List el -> List el
updateOrAddOrdered compareIdentifier updateEl getEmptyEl sortedList =
    let
        ( left, right ) =
            List.partition (compareIdentifier >> (==) GT) sortedList
    in
    case right of
        -- add new el to the end
        [] ->
            List.append left [ updateEl <| getEmptyEl () ]

        el :: rest ->
            case compareIdentifier el of
                -- update the el
                EQ ->
                    List.concat [ left, [ updateEl el ], rest ]

                -- add new el between left and right
                _ ->
                    List.concat [ left, [ updateEl <| getEmptyEl () ], right ]


{-| If the set does not contain the element, add it. If it does contain the element, remove it.

    toggle 1 (Set.fromList [1,2,3])
    --> Set.fromList [2, 3]
    toggle 1 (Set.fromList [2,3])
    --> Set.fromList [1, 2, 3]

-}
toggle : comparable -> Set comparable -> Set comparable
toggle elem set =
    if Set.member elem set then
        Set.remove elem set

    else
        Set.insert elem set
