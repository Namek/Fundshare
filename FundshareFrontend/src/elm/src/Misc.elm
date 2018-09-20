module Misc exposing (attrWhen, delay, edges, either, emailRegex, match, moneyRegex, noCmd, toggle, viewIf)

import Element exposing (Element)
import Html
import Html.Attributes
import Process
import Regex exposing (Regex)
import Set exposing (Set)
import Task
import Time exposing (Posix)


match : Maybe Regex -> String -> Bool
match maybeRegex str =
    maybeRegex
        |> Maybe.andThen
            (\regex ->
                Regex.find regex str
                    |> List.any (.match >> (==) str)
                    |> Just
            )
        |> Maybe.withDefault False


moneyRegex : Maybe Regex
moneyRegex =
    Regex.fromString "[0-9]+([,.]?[0-9]?[0-9]?)?"


emailRegex : Maybe Regex
emailRegex =
    Regex.fromString ".{3,}@.{2,}"


viewIf : Bool -> Element msg -> Element msg
viewIf cond el =
    if cond then
        el

    else
        Element.none


either : a -> a -> Bool -> a
either a1 a2 cond =
    if cond then
        a1

    else
        a2


delay : Float -> msg -> Cmd msg
delay milliseconds msg =
    Process.sleep milliseconds
        |> Task.perform (always msg)


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


edges =
    { top = 0
    , right = 0
    , bottom = 0
    , left = 0
    }


noCmd : model -> ( model, Cmd msg )
noCmd model =
    ( model, Cmd.none )


attrWhen : Bool -> Element.Attribute msg -> Element.Attribute msg
attrWhen condition attr =
    if condition then
        attr

    else
        Element.htmlAttribute (Html.Attributes.attribute "empty-attr" "")
