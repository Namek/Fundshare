module Misc exposing
    ( attr
    , attrWhen
    , css
    , delay
    , edges
    , either
    , emailRegex
    , match
    , moneyRegex
    , noCmd
    , noShadow
    , styledButton
    , toggle
    , userSelectNone
    , viewBadge
    , viewIcon
    , viewIconButton
    , viewIf
    )

import Bitwise exposing (and, shiftLeftBy, shiftRightBy)
import Element exposing (Element, centerX, centerY, column, el, fill, focused, height, mouseDown, padding, px, rgb, rgb255, rgba, rgba255, row, shrink, spaceEvenly, text, width)
import Element.Background as Bg
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Html
import Html.Attributes
import Misc.Colors exposing (blueGray200, gray100, gray500, teal100, teal200, teal700, white)
import Process
import Regex exposing (Regex)
import Set exposing (Set)
import Task
import Time exposing (Posix)


match : Regex -> String -> Bool
match regex str =
    Regex.find regex str
        |> List.any (.match >> (==) str)


moneyRegex : Regex
moneyRegex =
    Regex.fromString "[0-9]+([,.]?[0-9]?[0-9]?)?"
        |> Maybe.withDefault Regex.never


emailRegex : Regex
emailRegex =
    Regex.fromString ".{3,}@.{2,}"
        |> Maybe.withDefault Regex.never


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


attr : String -> String -> Element.Attribute msg
attr name value =
    Element.htmlAttribute (Html.Attributes.attribute name value)


attrWhen : Bool -> Element.Attribute msg -> Element.Attribute msg
attrWhen condition otherAttr =
    if condition then
        otherAttr

    else
        attr "empty-attr" ""


noShadow =
    Border.shadow { offset = ( 0, 0 ), size = 0, blur = 0, color = rgba 0 0 0 0 }


styledButton : List (Element.Attribute msg) -> { onPress : Maybe msg, label : Element msg } -> Element msg
styledButton attrs opts =
    let
        disabled =
            opts.onPress == Nothing

        enabled =
            not disabled

        allAttrs =
            List.append
                [ enabled |> either teal700 teal100 |> Bg.color
                , enabled |> either (Border.shadow { offset = ( 1, 2 ), size = 1, blur = 7, color = rgba 0 0 0 0.2 }) noShadow
                , Font.color white
                , Font.size 14
                , padding 5
                , Border.rounded 3
                ]
                attrs
    in
    Input.button allAttrs opts


viewIcon attrs name =
    el
        (List.append [ attr "class" ("icon-" ++ name) ] attrs)
        (Element.text "")


viewIconButton : List (Element.Attribute msg) -> String -> msg -> Element msg
viewIconButton attrs iconName clickMsg =
    Input.button
        [ Font.color white
        , mouseDown [ noShadow, Font.color teal100 ]
        , focused [ noShadow ]
        ]
        { onPress = Just clickMsg
        , label =
            column
                (List.append [ spaceEvenly ] attrs)
                {- some workarounds to center the <i> icon -}
                [ el [] (text "")
                , row [ width fill, spaceEvenly ]
                    [ el [] (text "")
                    , viewIcon [ width fill, height shrink ] iconName
                    , el [] (text "")
                    ]
                , el [] (text "")
                ]
        }


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
