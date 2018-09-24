module Misc exposing
    ( attrWhen
    , css
    , delay
    , edges
    , either
    , emailRegex
    , grayed
    , match
    , moneyRegex
    , noCmd
    , noShadow
    , teal100
    , teal500
    , teal700
    , toggle
    , userSelectNone
    , viewBadge
    , viewIcon
    , viewIf
    , white
    )

import Element exposing (Element, centerX, centerY, fill, height, padding, px, rgb, rgb255, rgba, shrink, width)
import Element.Background as Bg
import Element.Border as Border
import Element.Font as Font
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


css property value =
    Element.htmlAttribute (Html.Attributes.style property value)


attrWhen : Bool -> Element.Attribute msg -> Element.Attribute msg
attrWhen condition attr =
    if condition then
        attr

    else
        Element.htmlAttribute (Html.Attributes.attribute "empty-attr" "")


teal500 =
    rgb255 0x00 0x91 0x83


teal700 =
    rgb255 0x00 0x79 0x6B


teal100 =
    rgb255 0xB2 0xDF 0xDB


white =
    rgb255 0xFF 0xFF 0xFF


grayed =
    rgb255 0xB2 0xDE 0xDA


noShadow =
    Border.shadow { offset = ( 0, 0 ), size = 0, blur = 0, color = rgba 0 0 0 0 }


viewIcon name =
    Element.html <|
        Html.i [ Html.Attributes.class ("icon-" ++ name) ] [ Html.text "" ]


viewBadge aText =
    Element.row
        [ Bg.color (rgb255 255 82 82)
        , Border.rounded 100
        , Font.color white
        , Font.size 13
        , Font.semiBold
        , width (px 18)
        , height (px 18)
        ]
        [ Element.column [ height shrink, width shrink, centerX, centerY ]
            [ Element.text aText ]
        ]


userSelectNone =
    css "user-select" "none"
