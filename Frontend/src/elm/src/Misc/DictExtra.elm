module Misc.DictExtra exposing (getMultipleValues, setMultipleValues)

import Dict exposing (Dict)


getMultipleValues : Dict Int data -> Int -> Int -> List (Maybe data)
getMultipleValues dict startIndex limit =
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


setMultipleValues : Dict Int data -> Int -> List data -> Dict Int data
setMultipleValues dict startIndex values =
    case values of
        [] ->
            dict

        x :: xs ->
            let
                newDict =
                    Dict.insert startIndex x dict
            in
            setMultipleValues newDict (startIndex + 1) xs
