module Views.Timeline exposing (Model, Msg, init, insertTransactions, update, view)

import Cmd.Extra
import Data.Context exposing (ContextData, GlobalMsg, Logged)
import Data.Session exposing (Session)
import Data.Transaction exposing (Transaction, amountDifferenceForMyAccount, amountToMoney, amountToMoneyChange, amountToMoneyLeftPad)
import Date exposing (Date)
import Dict exposing (Dict)
import Element exposing (Element, alignTop, behindContent, centerX, centerY, column, fill, height, inFront, link, moveDown, moveRight, paddingEach, paddingXY, paragraph, px, row, spacing, text, width)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font exposing (center, justify)
import Element.Input as Input
import Graphql.Http
import Html exposing (i)
import List exposing (range)
import List.Extra
import Misc exposing (css, edges, either, getUpdatedProperty, noCmd, noShadow, viewIf)
import Misc.Colors as Colors exposing (red50)
import Misc.DataExtra exposing (updateOrAddOrdered)
import Route
import Set exposing (Set)
import Time exposing (Posix)



-- MODEL --


type alias Model =
    Timeline


type alias Timeline =
    { -- grouped and sorted by date DESC
      dayViews : List DayView
    }


type alias DayView =
    { date : Date
    , transactionViews : List TransactionView
    , isExpanded : Bool
    }


type alias TransactionView =
    { transaction : Transaction
    }


init : List Transaction -> Model
init transactions =
    insertTransactions { dayViews = [] } transactions


insertTransactions : Timeline -> List Transaction -> Timeline
insertTransactions timeline transactions =
    case transactions of
        [] ->
            timeline

        transaction :: rest ->
            let
                dayDate =
                    transaction.insertedAt |> Date.fromPosix Time.utc

                transactionTime =
                    transaction.insertedAt |> Time.posixToMillis

                updateDayView dayView =
                    let
                        updatedTransactionViews : List TransactionView
                        updatedTransactionViews =
                            dayView.transactionViews
                                |> updateOrAddOrdered
                                    (\tv ->
                                        let
                                            cmp =
                                                compare
                                                    transactionTime
                                                    (tv.transaction.insertedAt |> Time.posixToMillis)
                                        in
                                        if cmp == EQ then
                                            compare transaction.id tv.transaction.id

                                        else
                                            cmp
                                    )
                                    (\tv -> { tv | transaction = transaction })
                                    (\() -> { transaction = transaction })
                    in
                    { dayView | transactionViews = updatedTransactionViews }

                updatedDayViews : List DayView
                updatedDayViews =
                    updateOrAddOrdered
                        (\dv -> Date.compare dv.date dayDate)
                        updateDayView
                        (\() -> { date = dayDate, transactionViews = [], isExpanded = False })
                        timeline.dayViews

                updatedTimeline : Timeline
                updatedTimeline =
                    { dayViews = updatedDayViews }
            in
            insertTransactions updatedTimeline rest


type alias Context msg =
    Logged (ContextData Model Msg msg)



-- UPDATE --


type Msg
    = NoOp
    | ToggleDayView DayView


type alias MsgLift msg =
    Msg -> msg


update : Model -> Msg -> ( Model, Cmd Msg )
update model msg =
    case msg of
        NoOp ->
            model |> noCmd

        ToggleDayView dayView ->
            let
                updatedDayViews =
                    List.Extra.updateIf
                        (\dv -> EQ == Date.compare dv.date dayView.date)
                        (\dv -> { dv | isExpanded = not dv.isExpanded })
                        model.dayViews

                updatedModel =
                    { model | dayViews = updatedDayViews }
            in
            updatedModel |> noCmd



-- VIEW --


view : Context msg -> Element msg
view ctx =
    let
        { model } =
            ctx

        isFirstDayToday =
            List.head model.dayViews
                |> Maybe.andThen (\dv -> Just <| Date.compare dv.date ctx.todayDate == EQ)
                |> Maybe.withDefault False
    in
    column [ Font.size 15 ] <|
        List.concat
            [ [ if not isFirstDayToday then
                    row
                        [ inFront <|
                            Element.el
                                [ moveRight <| leftSideWidth - 4
                                , moveDown <| 4
                                , css "z-index" "2"
                                ]
                            <|
                                viewDayPoint ctx
                        ]
                        [ text "Today"
                        ]

                else
                    Element.none
              ]
            , List.map (viewDay ctx) model.dayViews
            ]


viewDay : Context msg -> DayView -> Element msg
viewDay ctx dayView =
    let
        transactionViews =
            dayView.transactionViews
    in
    row
        [ paddingEach { edges | top = eachDaySpacing }
        , behindContent <|
            -- vertical line
            Element.el
                [ moveRight leftSideWidth
                , width (px middlePartWidth)
                , height fill
                , Border.color Colors.gray300
                , Border.widthEach { edges | left = 3 }
                , Border.solid
                ]
                (text "")
        , inFront <|
            Element.el
                [ moveRight <| leftSideWidth - 4
                , moveDown <| eachDaySpacing + 4
                ]
            <|
                viewDayPoint ctx
        ]
        [ column
            [ alignTop
            , width (px leftSideWidth)
            , Font.size 14
            , paddingEach { edges | top = 2 }
            ]
            [ Input.button [ noShadow ]
                { label = text <| dayRelative ctx.todayDate dayView.date
                , onPress = Just <| ctx.lift <| ToggleDayView dayView
                }
            ]
        , column
            [ alignTop
            , Font.size 14
            , paddingEach { edges | left = middlePartWidth, top = 2 }
            , spacing 4
            ]
          <|
            (if List.length transactionViews > 1 || not dayView.isExpanded then
                let
                    daySum =
                        transactionViews
                            |> List.foldl
                                (.transaction
                                    >> amountDifferenceForMyAccount ctx.session.user.id
                                    >> (+)
                                )
                                0
                in
                Element.el
                    (if List.length transactionViews > 1 && dayView.isExpanded then
                        [ Border.widthEach { edges | bottom = 1 }
                        , Border.color Colors.gray300
                        , paddingEach { edges | bottom = 3 }
                        , width <| px 75
                        ]

                     else
                        [ width <| px 75 ]
                    )
                <|
                    Element.el
                        [ Font.family [ Font.monospace ]
                        , Font.color <| either Colors.green800 Colors.red500 (daySum > 0)
                        ]
                        (text <| amountToMoneyLeftPad True maxIntegralDigits daySum)

             else
                Element.none
            )
                :: (if dayView.isExpanded then
                        List.map (viewTransaction ctx) transactionViews

                    else
                        []
                   )
        ]


viewDayPoint : Context msg -> Element msg
viewDayPoint ctx =
    let
        size =
            11
    in
    Element.el
        [ width (px size)
        , height (px size)
        , Background.color Colors.gray300
        , Border.rounded size
        , Border.color Colors.blueGray300
        , Border.solid
        , Border.width 1
        , css "z-index" "2"
        , Font.color Colors.white
        , Font.size (size - 2)
        , center
        ]
        (text "")


viewTransaction : Context msg -> TransactionView -> Element msg
viewTransaction ctx tv =
    let
        t =
            tv.transaction

        diff =
            amountDifferenceForMyAccount ctx.session.user.id t
    in
    Element.row []
        [ Element.el
            [ Font.family [ Font.monospace ]
            , Font.color <| either Colors.green800 Colors.red500 (diff > 0)
            , width (px <| (maxIntegralDigits + 4) * 10)
            ]
            (diff
                |> amountToMoneyLeftPad True maxIntegralDigits
                |> text
            )
        , Element.el [] (viewTransactionTags ctx tv)
        ]


viewTransactionTags : Context msg -> TransactionView -> Element msg
viewTransactionTags ctx tv =
    Element.row [ spacing 3 ] <|
        List.map (\tag -> viewTag ctx tv tag) tv.transaction.tags


viewTag : Context msg -> TransactionView -> String -> Element msg
viewTag ctx tv tag =
    Element.el
        [ Border.rounded 3
        , Background.color Colors.teal200
        , Font.color Colors.white
        , paddingXY 4 2
        , Font.size 12
        ]
    <|
        text tag


middlePartWidth =
    18


leftSideWidth =
    110


eachDaySpacing =
    20


maxIntegralDigits =
    5


dayRelative : Date -> Date -> String
dayRelative today date =
    let
        diff =
            Date.diff Date.Days date today
    in
    case diff of
        0 ->
            "Today"

        1 ->
            "Yesterday"

        n ->
            (n |> String.fromInt) ++ " days ago"
