module Views.Timeline exposing (Model, Msg, init, update, view)

import Data.Context exposing (ContextData, GlobalMsg, Logged)
import Data.Person as Person exposing (Person)
import Data.Transaction exposing (Transaction, amountDifferenceForMyAccount, amountToMoney, amountToMoneyChange, amountToMoneyChangeLeftPad)
import Date as Date exposing (Date)
import Dict exposing (Dict)
import Element exposing (Element, above, alignRight, alignTop, behindContent, below, centerX, centerY, column, fill, height, inFront, link, mouseOver, moveDown, moveLeft, moveRight, padding, paddingEach, paddingXY, paragraph, px, row, spacing, text, width)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font exposing (center, justify)
import Element.Input as Input
import Element.Lazy
import I18n.I18n as I18n
import List
import Misc exposing (attrWhen, css, dayRelative, edges, either, getUpdatedProperty, noCmd, noShadow, userSelectNone, viewIf)
import Misc.Colors as Colors exposing (red50)
import Misc.DataExtra exposing (updateOrAddOrdered)
import Time exposing (Posix)


type alias Context msg =
    Logged (ContextData Model Msg msg)



-- MODEL --


type alias Model =
    { timeline : Timeline
    }


type alias Timeline =
    { -- the key is Int made from Date, since Date cannot be a key in Dict
      dayViewsExpanded : Dict Int Bool
    }


init : ( Model, Cmd Msg )
init =
    ( { timeline = emptyTimeline }
    , Cmd.none
    )


emptyTimeline : Timeline
emptyTimeline =
    { dayViewsExpanded = Dict.empty }


type alias DayView =
    { date : Date
    , isExpanded : Bool
    , transactionsSorted : List Transaction
    }


prepareDayViews : List DayView -> Timeline -> List Transaction -> List DayView
prepareDayViews currentDayViews timeline transactions =
    case transactions of
        [] ->
            currentDayViews

        transaction :: rest ->
            let
                dayDate =
                    transaction.insertedAt |> Date.fromPosix Time.utc

                dayDateAsNumber =
                    Date.toRataDie dayDate

                transactionTime =
                    transaction.insertedAt |> Time.posixToMillis

                updateDayView dayView =
                    let
                        updatedTransactions : List Transaction
                        updatedTransactions =
                            dayView.transactionsSorted
                                |> updateOrAddOrdered
                                    (\t ->
                                        let
                                            cmp =
                                                compare
                                                    transactionTime
                                                    (t.insertedAt |> Time.posixToMillis)
                                        in
                                        if cmp == EQ then
                                            compare transaction.id t.id

                                        else
                                            cmp
                                    )
                                    (\t -> transaction)
                                    (\() -> transaction)
                    in
                    { dayView
                        | transactionsSorted = updatedTransactions
                        , isExpanded =
                            Dict.get dayDateAsNumber timeline.dayViewsExpanded
                                |> Maybe.withDefault True
                    }

                updatedDayViews : List DayView
                updatedDayViews =
                    updateOrAddOrdered
                        (\dv -> Date.compare dv.date dayDate)
                        updateDayView
                        (\() ->
                            { date = dayDate
                            , transactionsSorted = []
                            , isExpanded = True
                            }
                        )
                        currentDayViews
            in
            prepareDayViews updatedDayViews timeline rest



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
                timeline =
                    model.timeline

                dateAsNumber : Int
                dateAsNumber =
                    Date.toRataDie dayView.date

                updatedDayViews =
                    Dict.update dateAsNumber
                        (\maybeIsExpanded ->
                            case maybeIsExpanded of
                                Just isExpanded ->
                                    Just <| not isExpanded

                                Nothing ->
                                    Just False
                        )
                        timeline.dayViewsExpanded
            in
            { model | timeline = { timeline | dayViewsExpanded = updatedDayViews } } |> noCmd



-- VIEW --


view : Context msg -> List Transaction -> Element msg
view ctx transactions =
    Element.Lazy.lazy (view2 ctx) transactions


view2 : Context msg -> List Transaction -> Element msg
view2 ctx transactions =
    let
        { model } =
            ctx

        dayViews =
            prepareDayViews [] model.timeline transactions

        isFirstDayToday =
            List.head dayViews
                |> Maybe.andThen (\dv -> Just <| Date.compare dv.date ctx.todayDate == EQ)
                |> Maybe.withDefault False
    in
    column [ Font.size 15, paddingEach { edges | top = 15 } ] <|
        List.concat
            [ [ if not isFirstDayToday then
                    paragraph
                        [ inFront <|
                            Element.el
                                [ moveRight <| leftSideWidth - 4
                                , moveDown <| 4
                                , css "z-index" "2"
                                ]
                            <|
                                viewDayPoint ctx
                        , width (px leftSideWidth)
                        , Font.alignRight
                        , paddingEach { edges | right = 15 }
                        ]
                        [ text "Today"
                        ]

                else
                    Element.none
              ]
            , List.map (viewDay ctx) dayViews
            ]


viewDay : Context msg -> DayView -> Element msg
viewDay ctx dayView =
    let
        transactionViews =
            dayView.transactionsSorted
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
                , Border.widthEach { edges | left = 2 }
                , Border.solid
                ]
                (text "")
        , inFront <|
            Element.el
                [ moveRight <| leftSideWidth - 4.5
                , moveDown <| eachDaySpacing + 4
                ]
            <|
                viewDayPoint ctx
        ]
        [ paragraph
            [ alignTop
            , width (px leftSideWidth)
            , Font.size 14
            , paddingEach { edges | top = 2, right = 15 }
            , Font.alignRight
            ]
            [ Input.button [ noShadow ]
                { label = text <| dayRelative ctx.todayDate dayView.date
                , onPress = Just <| ctx.lift <| ToggleDayView dayView
                }
            , Element.row
                [ padding 4
                , Background.color Colors.white
                , Font.alignRight
                , Font.size 10
                ]
              <|
                [ text <|
                    Date.toIsoString <|
                        dayView.date
                ]
            ]
        , column
            [ alignTop
            , Font.size 14
            , paddingEach { edges | left = middlePartWidth, top = 2 }
            , spacing <| either 10 3 dayView.isExpanded
            ]
          <|
            (if List.length transactionViews > 1 then
                let
                    daySum =
                        transactionViews
                            |> List.foldl
                                (identity
                                    >> amountDifferenceForMyAccount ctx.session.id
                                    >> (+)
                                )
                                0
                in
                Element.el
                    (if List.length transactionViews > 1 then
                        [ Border.widthEach { edges | bottom = 1 }
                        , Border.color Colors.gray300
                        , paddingEach { edges | bottom = 3 }
                        , width <| px transactionMoneyColumnWidth
                        ]

                     else
                        [ width <| px transactionMoneyColumnWidth ]
                    )
                    (Element.el
                        [ Font.family [ Font.monospace ]
                        , Font.color <| either Colors.green800 Colors.red500 (daySum > 0)
                        , paddingEach { edges | left = 7 }
                        ]
                        (text <| amountToMoneyChangeLeftPad True maxIntegralDigits daySum ++ " " ++ I18n.currency.suffixOrCode)
                    )

             else
                Element.none
            )
                :: List.map (viewTransaction ctx dayView.isExpanded) transactionViews
        ]


{-| The graphical circle element on vertical line that represents timeline.
-}
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
        , Border.solid
        , Border.width 1
        , css "z-index" "2"
        , Font.color Colors.white
        , Font.size (size - 2)
        , center
        ]
        (text "")


viewTransaction : Context msg -> Bool -> Transaction -> Element msg
viewTransaction ctx isExpanded t =
    let
        isCollapsed =
            not isExpanded

        diff =
            amountDifferenceForMyAccount ctx.session.id t

        basics =
            Element.row
                []
                [ Element.row
                    [ Font.family [ Font.monospace ]
                    , width (px <| transactionMoneyColumnWidth)
                    ]
                    [ diff
                        |> amountToMoneyChangeLeftPad True (isExpanded |> either 0 maxIntegralDigits)
                        |> text
                        |> Element.el [ Font.color <| either Colors.green800 Colors.red500 (diff > 0) ]
                    , viewIf (isExpanded && t.amount /= abs diff) <|
                        (Element.el [ Font.color Colors.gray400 ] <| text <| " / " ++ String.fromFloat (amountToMoney t.amount))
                    ]
                , viewIf isCollapsed <| viewTransactionTags ctx False t
                ]

        personIdToName : Int -> String
        personIdToName pid =
            Person.personIdToName ctx.commonData.people pid

        viewDetails () =
            column [ paddingXY 0 7, spacing 7 ]
                [ row [ width <| px transactionMoneyColumnWidth ]
                    [ text <| personIdToName t.payorId
                    , text <| " → "
                    , text <| String.join ", " <| List.map personIdToName t.beneficientIds
                    ]
                , paragraph [ Font.size 13, Font.color Colors.gray500 ]
                    [ text <| Maybe.withDefault "" t.description ]
                ]
    in
    Element.column
        [ Border.color Colors.teal50
        , Border.width 1 |> attrWhen isExpanded
        , Border.rounded 3
        , width fill
        , mouseOver [ Background.color Colors.teal50 ]
        , paddingXY 7 (either 7 0 <| isExpanded)
        , width (px 250) |> attrWhen isExpanded
        , userSelectNone
        , inFront (Element.el [ alignRight, moveLeft 6, moveDown 4 ] <| viewTransactionTags ctx True t) |> attrWhen isExpanded
        ]
        [ basics
        , if isExpanded then
            viewDetails ()

          else
            Element.none
        ]


viewTransactionTags : Context msg -> Bool -> Transaction -> Element msg
viewTransactionTags ctx inColumn transaction =
    (inColumn |> either column row)
        [ spacing 3 ]
        (List.map viewTag transaction.tags)


viewTag : String -> Element msg
viewTag tag =
    Element.el
        [ Border.rounded 3
        , Background.color Colors.teal200
        , Font.color Colors.white
        , paddingXY 4 2
        , Font.size 12
        , alignRight
        ]
        (text tag)


middlePartWidth =
    18


leftSideWidth =
    110


eachDaySpacing =
    20


maxIntegralDigits =
    5


transactionMoneyColumnWidth =
    115
