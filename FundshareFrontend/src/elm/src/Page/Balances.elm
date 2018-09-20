module Page.Balances exposing (Model, Msg, init, update, view)

import Cmd.Extra
import Data.Balance exposing (Balance)
import Data.Context exposing (..)
import Data.Person exposing (Person)
import Data.Session exposing (Session)
import Element exposing (Element, alignRight, alignTop, centerY, column, el, fill, maximum, padding, px, rgb, rgb255, row, spaceEvenly, text, width, wrappedRow)
import Element.Background as Bg
import Element.Font as Font
import Json.Decode as Json
import List.Extra
import Maybe.Extra
import Misc exposing (either, viewIf)
import Misc.Time exposing (timeDistanceInWords)
import Request.Balance exposing (getBalances)
import Request.Common exposing (..)
import Task
import Time exposing (Posix, now)



-- MODEL --


type alias Model =
    { balances : List Balance
    , dateTimeNow : Maybe Posix
    }


init : Session -> ( Model, Cmd Msg )
init session =
    ( { balances = []
      , dateTimeNow = Nothing
      }
    , Cmd.batch
        [ Cmd.Extra.perform RefreshBalances
        , Task.attempt SetDate now
        ]
    )


type alias Context msg =
    Logged (ContextData Model Msg msg)



-- STATE UPDATE --


type Msg
    = RefreshBalances
    | RefreshBalancesResponse (Result Error (List Balance))
    | SetDate (Result String Posix)


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
            ( model, cmd )

        SetDate date ->
            ( date
                |> Result.andThen (\now -> Ok { model | dateTimeNow = Just now })
                |> Result.withDefault model
            , Cmd.none
            )

        RefreshBalancesResponse result ->
            ( result
                |> Result.andThen (\balances -> Ok { model | balances = balances })
                |> Result.withDefault model
            , Cmd.none
            )



-- VIEW --


view : Context msg -> Element msg
view ctx =
    let
        balances =
            ctx.model.balances
    in
    wrappedRow
        [ centerY ]
        ((viewBalanceSummaryCard ctx
            :: List.indexedMap (viewBalanceCard ctx) balances
         )
            |> List.map (Element.el [ padding 6 ])
        )


viewBalanceSummaryCard : Context msg -> Element msg
viewBalanceSummaryCard ctx =
    let
        balances =
            ctx.model.balances

        totalBalance =
            balances
                |> List.map
                    (\b ->
                        b.value * (b.iHaveMore |> either 1 -1)
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
                Font.color (rgb255 255 255 255)

            else
                iHaveMore
                    |> either
                        (Font.color (rgb255 255 255 255))
                        (Font.color (rgb255 255 245 157))

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
                        |> List.Extra.maximumBy Time.posixToMillis
            in
            case ( lastUpdateAt, ctx.model.dateTimeNow ) of
                ( Just last, Just now ) ->
                    timeDistanceInWords True last now ++ " ago"

                _ ->
                    "no changes yet"
    in
    row
        [ width (fill |> maximum 350)
        , Bg.color (rgb255 0 0x96 0x88)
        ]
        [ row [ spaceEvenly, alignTop ]
            [ el [] <| text "Total"
            , el [ alignRight ] <| text "asd"
            ]
        ]



-- Card.view
--     [ css "max-width" "350px"
--     , Color.background (Color.color Color.Teal Color.S500)
--     , Elevation.e4
--     ]
--     [ Card.title
--         [ css "align-content" "flex-start"
--         , css "flex-direction" "row"
--         , css "align-items" "flex-start"
--         , css "justify-content" "space-between"
--         ]
--         [ Options.div
--             []
--             [ Card.head
--                 [ white, css "align-items" "baseline" ]
--                 [ Options.span [] [ text "Total" ]
--                 ]
--             , Card.subhead [ white ]
--                 [ Options.styled p
--                     [ css "opacity" "0.7"
--                     , css "margin-top" "4pt"
--                     , css "margin-bottom" "0"
--                     ]
--                     [ text (String.fromInt sharedPaymentCount ++ " shared payments")
--                     , Html.br [] []
--                     , text (String.fromInt transferCount ++ " transfers")
--                     ]
--                 ]
--             ]
--         , Options.div
--             [ css "align-self" "flex-end"
--             , css "margin-bottom" "-2pt"
--             ]
--             [ viewIf (totalBalance /= 0)
--                 (Options.span
--                     [ Typo.display1
--                     , Color.text comparedColor
--                     ]
--                     [ text (valueSign ++ " ") ]
--                 )
--             , Options.span
--                 [ css "opacity" "1"
--                 , Typo.display2
--                 , Color.text comparedColor
--                 ]
--                 [ text (totalBalance |> abs |> String.fromFloat) ]
--             , Options.span
--                 [ Typo.subhead, white ]
--                 [ text " zł" ]
--             ]
--         ]
--     , Card.actions
--         [ Card.border
--         , css "display" "flex"
--         , css "justify-content" "space-between"
--         , css "align-items" "center"
--         , css "padding" "8px 16px 8px 16px"
--         , white
--         ]
--         [ Options.span
--             [ Typo.caption
--             , Color.text Color.white
--             , css "display" "inline-flex"
--             , css "align-items" "center"
--             ]
--             [ Options.span
--                 [ css "margin-right" "4px" ]
--                 [ Icon.i "update" ]
--             , text lastChangeDatetime
--             ]
--         , Options.span []
--             [ Button.render ctx.liftMaterial
--                 (ctx.matId [ 1, 0, 0 ])
--                 ctx.mdl
--                 [ Button.icon, Button.ripple, white ]
--                 [ Icon.i "add" ]
--             , Button.render ctx.liftMaterial
--                 (ctx.matId [ 1, 0, 1 ])
--                 ctx.mdl
--                 [ Button.icon, Button.ripple, white, css "margin-left" "4pt" ]
--                 [ Icon.view "event" [] ]
--             , viewIf (unseenUpdateCount > 0) <|
--                 Options.span [ Badge.add (String.fromInt unseenUpdateCount), Badge.overlap ] []
--             ]
--         ]
--     ]


viewBalanceCard : Context msg -> Int -> Balance -> Element msg
viewBalanceCard ctx index balance =
    let
        value =
            balance.value

        iHaveMore =
            balance.iHaveMore

        valueSign =
            iHaveMore |> either "+" "-"

        comparedColor =
            if value == 0 then
                Font.color (rgb255 255 255 255)

            else
                iHaveMore
                    |> either
                        (Font.color (rgb255 255 255 255))
                        (Font.color (rgb255 255 245 157))

        -- textSize =
        --     if value == 0 then
        --         { sign = css "opacity" "0"
        --         , value = Typo.display2
        --         }
        --
        --     else if value >= 1000 then
        --         { sign = Typo.display1
        --         , value = Typo.display2
        --         }
        --
        --     else
        --         { sign = Typo.display1
        --         , value = Typo.display2
        --         }
        lastChangeDatetime =
            case ( balance.lastUpdateAt, ctx.model.dateTimeNow ) of
                ( Just last, Just now ) ->
                    Misc.Time.timeDistanceInWords True last now ++ " ago"

                _ ->
                    "no changes yet"
    in
    row [] []



-- Card.view
--     [ css "max-width" "350px"
--     , Color.background (Color.color Color.Teal Color.S700)
--     , Elevation.e4
--     ]
--     [ Card.title
--         [ css "align-content" "flex-start"
--         , css "flex-direction" "row"
--         , css "align-items" "flex-start"
--         , css "justify-content" "space-between"
--         ]
--         [ Options.div
--             []
--             [ Card.head
--                 [ white, css "align-items" "baseline" ]
--                 [ Options.span
--                     [ css "opacity" "0.8"
--                     , css "font-size" "0.8em"
--                     ]
--                     [ text "with" ]
--                 , Options.span
--                     [ css "margin-left" "5px" ]
--                     [ text balance.name ]
--                 ]
--             , Card.subhead [ white ]
--                 [ Options.styled p
--                     [ css "opacity" "0.7"
--                     , css "margin-top" "4pt"
--                     , css "margin-bottom" "0"
--                     ]
--                     [ text (String.fromInt balance.sharedPaymentCount ++ " shared payments")
--                     , Html.br [] []
--                     , text (String.fromInt balance.transferCount ++ " transfers")
--                     ]
--                 ]
--             ]
--         , Options.div
--             [ css "align-self" "flex-end"
--             , css "margin-bottom" "-2pt"
--             ]
--             [ Options.span
--                 [ textSize.sign, Color.text comparedColor ]
--                 [ text (valueSign ++ " ") ]
--             , Options.span
--                 [ css "opacity" "1"
--                 , textSize.value
--                 , Color.text comparedColor
--                 ]
--                 [ text (String.fromFloat value) ]
--             , Options.span
--                 [ Typo.subhead, white ]
--                 [ text " zł" ]
--             ]
--         ]
--     , Card.actions
--         [ Card.border
--         , css "display" "flex"
--         , css "justify-content" "space-between"
--         , css "align-items" "center"
--         , css "padding" "8px 16px 8px 16px"
--         , white
--         ]
--         [ Options.span
--             [ Typo.caption
--             , Color.text Color.white
--             , css "display" "inline-flex"
--             , css "align-items" "center"
--             ]
--             [ Options.span
--                 [ css "margin-right" "4px" ]
--                 [ Icon.i "update" ]
--             , text lastChangeDatetime
--             ]
--         , Options.span []
--             [ Button.render ctx.liftMaterial
--                 (ctx.matId [ 0, index, 0 ])
--                 ctx.mdl
--                 [ Button.icon, Button.ripple, white ]
--                 [ Icon.i "add" ]
--             , Button.render ctx.liftMaterial
--                 (ctx.matId [ 0, index, 1 ])
--                 ctx.mdl
--                 [ Button.icon, Button.ripple, white, css "margin-left" "4pt" ]
--                 [ Icon.view "event" [] ]
--             , viewIf (balance.unseenUpdateCount > 0) <|
--                 Options.span [ Badge.add (String.fromInt balance.unseenUpdateCount), Badge.overlap ] []
--             ]
--         ]
--     ]
-- white : Options.Property c m
-- white =
--     Color.text Color.white
