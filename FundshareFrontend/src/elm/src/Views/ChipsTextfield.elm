module Views.ChipsTextfield exposing (Config, Model, Msg, init, update, view)

import Cmd.Extra
import Data.Context exposing (ContextData)
import Element exposing (Color, Element, centerY, fill, focused, height, maximum, mouseDown, mouseOver, padding, paddingEach, paddingXY, px, row, spacing, text, width, wrappedRow)
import Element.Background as Bg
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Html.Events
import Json.Decode as Json
import List.Extra
import Misc exposing (attr, attrWhen, css, delay, edges, either, noShadow, userSelectNone, viewIf)
import Misc.Colors as Colors exposing (red400, teal100)
import Time


type alias Model =
    { text : String
    , chips : List String
    , animatedChip : Maybe Int
    }


type alias Config =
    { isEnabled : Bool
    , label : String
    , textWhenDisabled : String
    }


type alias Context msg =
    ContextData Model Msg msg


init : Model
init =
    { text = ""
    , chips = []
    , animatedChip = Nothing
    }


type Msg
    = SetText String
    | RemoveChip Int
    | OnTextfieldKeyDown Int
    | AnimateChipExists Int
    | AnimateChipExistsFinish


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SetText text ->
            ( { model | text = text }, Cmd.none )

        RemoveChip index ->
            let
                chips =
                    List.Extra.removeAt index model.chips
            in
            ( { model | chips = chips }, Cmd.none )

        OnTextfieldKeyDown keyCode ->
            case keyCode of
                13 ->
                    tryAddTag model

                _ ->
                    ( model, Cmd.none )

        AnimateChipExists index ->
            ( { model | animatedChip = Just index }
            , delay 200 <| AnimateChipExistsFinish
            )

        AnimateChipExistsFinish ->
            ( { model | animatedChip = Nothing }, Cmd.none )


tryAddTag : Model -> ( Model, Cmd Msg )
tryAddTag model =
    case String.trim model.text of
        "" ->
            ( model, Cmd.none )

        text ->
            case List.Extra.elemIndex text model.chips of
                Just index ->
                    ( model, Cmd.Extra.perform (AnimateChipExists index) )

                Nothing ->
                    ( { model | text = "", chips = text :: model.chips }, Cmd.none )


view : Context msg -> Config -> Element msg
view ctx cfg =
    let
        { model } =
            ctx
    in
    wrappedRow
        [ width (fill |> maximum 265), height (px 70), centerY, spacing 4 ]
        (List.append
            [ viewIf cfg.isEnabled (model.text |> viewTextfield ctx cfg)
            , viewIf (not cfg.isEnabled)
                (row [ paddingEach { edges | top = 20 } ] [ text cfg.textWhenDisabled ])
            ]
            (viewTagList ctx cfg.isEnabled)
        )


viewTextfield : Context msg -> Config -> String -> Element msg
viewTextfield ctx cfg text =
    Element.el
        [ centerY
        , Element.htmlAttribute <| Html.Events.on "keydown" (Json.map (ctx.lift << OnTextfieldKeyDown) Html.Events.keyCode)
        ]
        (Input.text
            [ width (px 100) ]
            { label = Input.labelHidden "Tags"
            , text = text
            , onChange = ctx.lift << SetText
            , placeholder = Just (Input.placeholder [] (Element.text "Add tag"))
            }
        )


viewTagList : Context msg -> Bool -> List (Element msg)
viewTagList ctx isEnabled =
    case ctx.model.chips of
        [] ->
            [ Element.none ]

        chips ->
            chips |> List.indexedMap (viewChip ctx ctx.model.animatedChip isEnabled)


viewChip : Context msg -> Maybe Int -> Bool -> Int -> String -> Element msg
viewChip ctx animatedIndex isEnabled index chip =
    let
        isFocused =
            Just index == animatedIndex
    in
    row
        [ userSelectNone
        , paddingXY 8 4
        , spacing 8
        , Bg.color Colors.teal200
        , Font.color Colors.white
        , Border.rounded 2
        , css "transition" "0.1s ease-out"
        , css "transform" "scale(1.1)" |> attrWhen isFocused
        , Bg.color Colors.red500 |> attrWhen isFocused
        ]
        [ text chip
        , Input.button
            [ focused [ noShadow ]
            , mouseOver [ noShadow, Bg.color red400 ]
            , mouseDown [ noShadow, Font.color teal100 ]
            , Border.rounded 4
            , padding 3
            ]
            { onPress =
                if isEnabled then
                    Just (ctx.lift <| RemoveChip index)

                else
                    Nothing
            , label =
                text "✖"
            }
        ]
