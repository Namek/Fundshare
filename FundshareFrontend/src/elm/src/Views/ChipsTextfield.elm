module Views.ChipsTextfield exposing (Config, Model, Msg, init, update, view)

import Cmd.Extra
import Data.Context exposing (ContextData)
import Html exposing (Html, beginnerProgram, text)
import Html.Events
import Json.Decode as Json
import List.Extra
import Material.Button as Button
import Material.Chip as Chip
import Material.Icon as Icon
import Material.Options as Opts exposing (css)
import Material.Textfield as Textfield
import Misc exposing ((=>), delay, either, viewIf)
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


view : Context msg -> Config -> Html msg
view ctx cfg =
    let
        { model } =
            ctx
    in
    Opts.div
        [ css "display" "inline-flex"
        , css "align-items" "center"
        , css "flex-wrap" "wrap"
        ]
        (Opts.div [ css "height" "70px" ]
            [ viewIf cfg.isEnabled (model.text |> viewTextfield ctx cfg)
            , viewIf (not cfg.isEnabled) (Opts.div [ css "padding-top" "20px" ] [ text cfg.textWhenDisabled ])
            ]
            :: viewTagList ctx cfg.isEnabled
        )


viewTextfield : Context msg -> Config -> String -> Html msg
viewTextfield ctx cfg text =
    Opts.div
        [ css "display" "inline-flex"
        , css "align-content" "center"
        , css "margin-right" "10px"
        ]
        [ Textfield.render ctx.liftMaterial
            (ctx.matId [ 1 ])
            ctx.mdl
            [ Textfield.label cfg.label
            , Textfield.value text
            , Opts.onInput (ctx.lift << SetText)
            , Opts.on "keydown" (Json.map (ctx.lift << OnTextfieldKeyDown) Html.Events.keyCode)
            , css "width" "60px"
            , css "margin-right" "10px"
            ]
            []
        ]


viewTagList : Context msg -> Bool -> List (Html msg)
viewTagList ctx isEnabled =
    case ctx.model.chips of
        [] ->
            [ Opts.styled Html.span [ css "margin-left" "10px" ] [ text "" ] ]

        chips ->
            chips |> List.indexedMap (viewChip ctx ctx.model.animatedChip isEnabled)


viewChip : Context msg -> Maybe Int -> Bool -> Int -> String -> Html msg
viewChip ctx animatedIndex isEnabled index chip =
    let
        isFocused =
            Just index == animatedIndex
    in
    Chip.span
        [ Chip.deleteClick (ctx.lift <| RemoveChip index) |> Opts.when isEnabled
        , css "user-select" "none"
        , css "transition" "0.1s ease-out"
        , css "transform" "scale(1.1)" |> Opts.when isFocused
        , css "color" "red" |> Opts.when isFocused
        , css "margin-right" "2px"
        ]
        [ Chip.content []
            [ text chip ]
        ]


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
            { model | text = text } => Cmd.none

        RemoveChip index ->
            let
                chips =
                    List.Extra.removeAt index model.chips
            in
            { model | chips = chips } => Cmd.none

        OnTextfieldKeyDown keyCode ->
            case keyCode of
                13 ->
                    tryAddTag model

                _ ->
                    model => Cmd.none

        AnimateChipExists index ->
            { model | animatedChip = Just index } => (delay (Time.millisecond * 200) <| AnimateChipExistsFinish)

        AnimateChipExistsFinish ->
            { model | animatedChip = Debug.log "ffff" Nothing } => Cmd.none


tryAddTag : Model -> ( Model, Cmd Msg )
tryAddTag model =
    case String.trim model.text of
        "" ->
            model => Cmd.none

        text ->
            case List.Extra.elemIndex text model.chips of
                Just index ->
                    model => Cmd.Extra.perform (AnimateChipExists index)

                Nothing ->
                    { model | text = "", chips = text :: model.chips } => Cmd.none
