module Page.Balances exposing (Model, Msg, init, update, view)

import Cmd.Extra
import Data.Balance exposing (Balance)
import Data.Context exposing (..)
import Data.Person exposing (Person)
import Data.Session exposing (Session)
import Date exposing (Date, now)
import Date.Distance
import Html exposing (Html, div, p, text)
import Html.Attributes
import Html.Events
import Json.Decode as Json
import List.Extra
import Material.Badge as Badge
import Material.Button as Button
import Material.Card as Card
import Material.Color as Color
import Material.Elevation as Elevation
import Material.Icon as Icon
import Material.List as Lists
import Material.Options as Options exposing (css, when)
import Material.Progress as Loading
import Material.Textfield as Textfield
import Material.Typography as Typo
import Maybe.Extra
import Misc exposing ((=>), either, viewIf)
import Request.Balance exposing (getBalances)
import Request.Common exposing (..)
import Task



-- MODEL --


type alias Model =
    { balances : List Balance
    , dateTimeNow : Maybe Date
    }


init : Session -> ( Model, Cmd Msg )
init session =
    { balances = []
    , dateTimeNow = Nothing
    }
        => Cmd.batch
            [ Cmd.Extra.perform RefreshBalances
            , Task.attempt SetDate Date.now
            ]


type alias Context msg =
    Logged (ContextData Model Msg msg)



-- VIEW --


view : Context msg -> Html msg
view ctx =
    let
        balances =
            ctx.model.balances
    in
    Options.div
        [ css "display" "flex"
        , css "flex-wrap" "wrap"
        , css "justify-content" "center"
        ]
        ((viewBalanceSummary ctx
            :: List.indexedMap (viewBalance ctx) balances
         )
            |> List.map
                (\el ->
                    Options.div
                        [ css "display" "inline-flex"
                        , css "padding" "6px"
                        ]
                        [ el ]
                )
        )


viewBalanceSummary : Context msg -> Html msg
viewBalanceSummary ctx =
    let
        balances =
            ctx.model.balances

        totalBalance =
            balances
                |> List.map
                    (\b ->
                        b.value
                            * (if b.iHaveMore then
                                1

                               else
                                -1
                              )
                    )
                |> List.sum
                |> (*) 100
                |> round
                |> toFloat
                |> (\a -> (/) a 100)

        iHaveMore =
            totalBalance > 0

        valueSign =
            iHaveMore |> either "+" "-"

        comparedColor =
            if totalBalance == 0 then
                Color.white

            else
                iHaveMore
                    |> either
                        Color.white
                        (Color.color Color.Yellow Color.S200)

        sharedPaymentCount =
            balances |> List.map .sharedPaymentCount |> List.sum

        transferCount =
            balances |> List.map .transferCount |> List.sum

        unseenUpdateCount =
            balances |> List.map .unseenUpdateCount |> List.sum

        lastChangeDatetime =
            let
                lastUpdateAt =
                    balances
                        |> List.filterMap .lastUpdateAt
                        |> List.Extra.maximumBy Date.toTime
            in
            case ( lastUpdateAt, ctx.model.dateTimeNow ) of
                ( Just last, Just now ) ->
                    Date.Distance.inWords last now ++ " ago"

                _ ->
                    "no changes yet"
    in
    Card.view
        [ css "max-width" "350px"
        , Color.background (Color.color Color.Teal Color.S500)
        , Elevation.e4
        ]
        [ Card.title
            [ css "align-content" "flex-start"
            , css "flex-direction" "row"
            , css "align-items" "flex-start"
            , css "justify-content" "space-between"
            ]
            [ Options.div
                []
                [ Card.head
                    [ white, css "align-items" "baseline" ]
                    [ Options.span [] [ text "Total" ]
                    ]
                , Card.subhead [ white ]
                    [ Options.styled p
                        [ css "opacity" "0.7"
                        , css "margin-top" "4pt"
                        , css "margin-bottom" "0"
                        ]
                        [ text (String.fromInt sharedPaymentCount ++ " shared payments")
                        , Html.br [] []
                        , text (String.fromInt transferCount ++ " transfers")
                        ]
                    ]
                ]
            , Options.div
                [ css "align-self" "flex-end"
                , css "margin-bottom" "-2pt"
                ]
                [ viewIf (totalBalance /= 0)
                    (Options.span
                        [ Typo.display1
                        , Color.text comparedColor
                        ]
                        [ text (valueSign ++ " ") ]
                    )
                , Options.span
                    [ css "opacity" "1"
                    , Typo.display2
                    , Color.text comparedColor
                    ]
                    [ text (totalBalance |> abs |> String.fromFloat) ]
                , Options.span
                    [ Typo.subhead, white ]
                    [ text " zł" ]
                ]
            ]
        , Card.actions
            [ Card.border
            , css "display" "flex"
            , css "justify-content" "space-between"
            , css "align-items" "center"
            , css "padding" "8px 16px 8px 16px"
            , white
            ]
            [ Options.span
                [ Typo.caption
                , Color.text Color.white
                , css "display" "inline-flex"
                , css "align-items" "center"
                ]
                [ Options.span
                    [ css "margin-right" "4px" ]
                    [ Icon.i "update" ]
                , text lastChangeDatetime
                ]
            , Options.span []
                [ Button.render ctx.liftMaterial
                    (ctx.matId [ 1, 0, 0 ])
                    ctx.mdl
                    [ Button.icon, Button.ripple, white ]
                    [ Icon.i "add" ]
                , Button.render ctx.liftMaterial
                    (ctx.matId [ 1, 0, 1 ])
                    ctx.mdl
                    [ Button.icon, Button.ripple, white, css "margin-left" "4pt" ]
                    [ Icon.view "event" [] ]
                , viewIf (unseenUpdateCount > 0) <|
                    Options.span [ Badge.add (String.fromInt unseenUpdateCount), Badge.overlap ] []
                ]
            ]
        ]


viewBalance : Context msg -> Int -> Balance -> Html msg
viewBalance ctx index balance =
    let
        value =
            balance.value

        iHaveMore =
            balance.iHaveMore

        valueSign =
            iHaveMore |> either "+" "-"

        comparedColor =
            if value == 0 then
                Color.white

            else
                iHaveMore
                    |> either
                        Color.white
                        (Color.color Color.Yellow Color.S200)

        textSize =
            if value == 0 then
                { sign = css "opacity" "0"
                , value = Typo.display2
                }

            else if value >= 1000 then
                { sign = Typo.display1
                , value = Typo.display2
                }

            else
                { sign = Typo.display1
                , value = Typo.display2
                }

        lastChangeDatetime =
            case ( balance.lastUpdateAt, ctx.model.dateTimeNow ) of
                ( Just last, Just now ) ->
                    Date.Distance.inWords last now ++ " ago"

                _ ->
                    "no changes yet"
    in
    Card.view
        [ css "max-width" "350px"
        , Color.background (Color.color Color.Teal Color.S700)
        , Elevation.e4
        ]
        [ Card.title
            [ css "align-content" "flex-start"
            , css "flex-direction" "row"
            , css "align-items" "flex-start"
            , css "justify-content" "space-between"
            ]
            [ Options.div
                []
                [ Card.head
                    [ white, css "align-items" "baseline" ]
                    [ Options.span
                        [ css "opacity" "0.8"
                        , css "font-size" "0.8em"
                        ]
                        [ text "with" ]
                    , Options.span
                        [ css "margin-left" "5px" ]
                        [ text balance.name ]
                    ]
                , Card.subhead [ white ]
                    [ Options.styled p
                        [ css "opacity" "0.7"
                        , css "margin-top" "4pt"
                        , css "margin-bottom" "0"
                        ]
                        [ text (String.fromInt balance.sharedPaymentCount ++ " shared payments")
                        , Html.br [] []
                        , text (String.fromInt balance.transferCount ++ " transfers")
                        ]
                    ]
                ]
            , Options.div
                [ css "align-self" "flex-end"
                , css "margin-bottom" "-2pt"
                ]
                [ Options.span
                    [ textSize.sign, Color.text comparedColor ]
                    [ text (valueSign ++ " ") ]
                , Options.span
                    [ css "opacity" "1"
                    , textSize.value
                    , Color.text comparedColor
                    ]
                    [ text (String.fromFloat value) ]
                , Options.span
                    [ Typo.subhead, white ]
                    [ text " zł" ]
                ]
            ]
        , Card.actions
            [ Card.border
            , css "display" "flex"
            , css "justify-content" "space-between"
            , css "align-items" "center"
            , css "padding" "8px 16px 8px 16px"
            , white
            ]
            [ Options.span
                [ Typo.caption
                , Color.text Color.white
                , css "display" "inline-flex"
                , css "align-items" "center"
                ]
                [ Options.span
                    [ css "margin-right" "4px" ]
                    [ Icon.i "update" ]
                , text lastChangeDatetime
                ]
            , Options.span []
                [ Button.render ctx.liftMaterial
                    (ctx.matId [ 0, index, 0 ])
                    ctx.mdl
                    [ Button.icon, Button.ripple, white ]
                    [ Icon.i "add" ]
                , Button.render ctx.liftMaterial
                    (ctx.matId [ 0, index, 1 ])
                    ctx.mdl
                    [ Button.icon, Button.ripple, white, css "margin-left" "4pt" ]
                    [ Icon.view "event" [] ]
                , viewIf (balance.unseenUpdateCount > 0) <|
                    Options.span [ Badge.add (String.fromInt balance.unseenUpdateCount), Badge.overlap ] []
                ]
            ]
        ]


white : Options.Property c m
white =
    Color.text Color.white


type Msg
    = RefreshBalances
    | RefreshBalancesResponse (Result Error (List Balance))
    | SetDate (Result String Date)


update : Context msg -> Msg -> ( Model, Cmd Msg )
update { model, session } msg =
    case msg of
        RefreshBalances ->
            let
                cmd =
                    getBalances
                        |> sendQueryRequest
                        |> Task.attempt RefreshBalancesResponse
            in
            model => cmd

        SetDate date ->
            (case date of
                Ok now ->
                    { model | dateTimeNow = Just now }

                _ ->
                    model
            )
                => Cmd.none

        RefreshBalancesResponse result ->
            case result of
                Ok balances ->
                    { model | balances = balances } => Cmd.none

                Err err ->
                    model => Cmd.none
