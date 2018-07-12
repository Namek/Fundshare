module Misc exposing (..)

import Html exposing (Html, text)
import Process
import Regex exposing (Regex, regex)
import Task
import Time exposing (Time)


(=>) : a -> b -> ( a, b )
(=>) =
    (,)


{-| infixl 0 means the (=>) operator has the same precedence as (<|) and (|>),
meaning you can use it at the end of a pipeline and have the precedence work out.
-}
infixl 0 =>


match : Regex -> String -> Bool
match rx str =
    Regex.find Regex.All rx str
        |> List.any (.match >> (==) str)


moneyRegex : Regex
moneyRegex =
    regex "[0-9]+([,.]?[0-9]?[0-9]?)?"


emailRegex : Regex
emailRegex =
    regex ".{3,}@.{2,}"


viewIf : Bool -> Html msg -> Html msg
viewIf cond el =
    if cond then
        el
    else
        text ""


either : a -> a -> Bool -> a
either a1 a2 cond =
    if cond then
        a1
    else
        a2


delay : Time.Time -> msg -> Cmd msg
delay time msg =
    Process.sleep time
        |> Task.andThen (always <| Task.succeed msg)
        |> Task.perform identity