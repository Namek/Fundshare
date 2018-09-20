module Views.ChipsTextfield exposing (Config, Model, Msg, init, update, view)

import Cmd.Extra
import Data.Context exposing (ContextData)
import Element exposing (Element, centerY, height, paddingEach, px, row, text, width, wrappedRow)
import Element.Input as Input
import Json.Decode as Json
import List.Extra
import Misc exposing (delay, edges, either, viewIf)
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


view : Context msg -> Config -> Element msg
view ctx cfg =
    let
        { model } =
            ctx
    in
    wrappedRow
        [ centerY ]
        (row [ height (px 70) ]
            [ viewIf cfg.isEnabled (model.text |> viewTextfield ctx cfg)
            , viewIf (not cfg.isEnabled)
                (row [ paddingEach { edges | top = 20 } ] [ text cfg.textWhenDisabled ])
            ]
            :: viewTagList ctx cfg.isEnabled
        )


viewTextfield : Context msg -> Config -> String -> Element msg
viewTextfield ctx cfg text =
    Element.el
        [ centerY ]
        (Input.text
            [ width (px 60) ]
            { label = Input.labelAbove [] (Element.text cfg.label)
            , text = text
            , onChange = ctx.lift << SetText
            , placeholder = Nothing
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
    Element.el []
        -- [ Chip.deleteClick (ctx.lift <| RemoveChip index) |> Opts.when isEnabled
        -- , css "user-select" "none"
        -- , css "transition" "0.1s ease-out"
        -- , css "transform" "scale(1.1)" |> Opts.when isFocused
        -- , css "color" "red" |> Opts.when isFocused
        -- , css "margin-right" "2px"
        -- ]
        (text chip)


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
            ( { model | animatedChip = Debug.log "ffff" Nothing }, Cmd.none )


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
