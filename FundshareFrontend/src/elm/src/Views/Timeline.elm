module Views.Timeline exposing (Model, Msg, init, insertTransactionsToModel, update, view)

import Cmd.Extra
import Data.Context exposing (ContextData, GlobalMsg, Logged)
import Data.Person exposing (Person)
import Data.Session exposing (Session)
import Data.Transaction exposing (Transaction, amountDifferenceForMyAccount, amountToMoney, amountToMoneyChange, amountToMoneyLeftPad)
import Date exposing (Date)
import Dict exposing (Dict)
import Element exposing (Element, alignRight, alignTop, behindContent, centerX, centerY, column, fill, height, inFront, link, mouseOver, moveDown, moveLeft, moveRight, padding, paddingEach, paddingXY, paragraph, px, row, spacing, text, width)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font exposing (center, justify)
import Element.Input as Input
import Graphql.Http
import Html exposing (i)
import List exposing (range)
import List.Extra
import Misc exposing (attrWhen, css, edges, either, getUpdatedProperty, noCmd, noShadow, userSelectNone, viewIf)
import Misc.Colors as Colors exposing (red50)
import Misc.DataExtra exposing (updateOrAddOrdered)
import RemoteData exposing (RemoteData)
import Request.Common exposing (sendQueryRequest)
import Request.People exposing (getPeople)
import Route
import Set exposing (Set)
import Time exposing (Posix)



-- MODEL --


type alias Model =
    { timeline : Timeline
    , people : List Person
    }


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


init : List Transaction -> List Person -> ( Model, Cmd Msg )
init transactions people =
    ( { timeline = insertTransactions { dayViews = [] } transactions
      , people = people
      }
    , getPeople |> sendQueryRequest RefreshPeopleList_Response
    )


insertTransactionsToModel : Model -> List Transaction -> Model
insertTransactionsToModel model transactions =
    let
        updatedTimeline =
            insertTransactions model.timeline transactions
    in
    { model | timeline = updatedTimeline }


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
                        (\() -> { date = dayDate, transactionViews = [], isExpanded = True })
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
    | RefreshPeopleList_Response (RemoteData (Graphql.Http.Error (List Person)) (List Person))
    | ToggleDayView DayView


type alias MsgLift msg =
    Msg -> msg


update : Model -> Msg -> ( Model, Cmd Msg )
update model msg =
    case msg of
        NoOp ->
            model |> noCmd

        RefreshPeopleList_Response result ->
            let
                newModel =
                    result
                        |> RemoteData.andThen (\people -> RemoteData.Success { model | people = people })
                        |> RemoteData.withDefault model
            in
            newModel |> noCmd

        ToggleDayView dayView ->
            let
                timeline =
                    model.timeline

                updatedDayViews =
                    List.Extra.updateIf
                        (\dv -> EQ == Date.compare dv.date dayView.date)
                        (\dv -> { dv | isExpanded = not dv.isExpanded })
                        timeline.dayViews

                updatedTimeline =
                    { timeline | dayViews = updatedDayViews }
            in
            { model | timeline = updatedTimeline } |> noCmd



-- VIEW --


view : Context msg -> Element msg
view ctx =
    let
        { model } =
            ctx

        isFirstDayToday =
            List.head model.timeline.dayViews
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
            , List.map (viewDay ctx) model.timeline.dayViews
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
                                (.transaction
                                    >> amountDifferenceForMyAccount ctx.session.user.id
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
                        (text <| amountToMoneyLeftPad True maxIntegralDigits daySum ++ " zł")
                    )

             else
                Element.none
            )
                :: List.map (viewTransaction ctx dayView.isExpanded) transactionViews
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
        , Border.solid
        , Border.width 1
        , css "z-index" "2"
        , Font.color Colors.white
        , Font.size (size - 2)
        , center
        ]
        (text "")


viewTransaction : Context msg -> Bool -> TransactionView -> Element msg
viewTransaction ctx isExpanded tv =
    let
        t =
            tv.transaction

        isCollapsed =
            not isExpanded

        diff =
            amountDifferenceForMyAccount ctx.session.user.id t

        basics =
            Element.row
                []
                [ Element.row
                    [ Font.family [ Font.monospace ]
                    , Font.color <| either Colors.green800 Colors.red500 (diff > 0)
                    , width (px <| transactionMoneyColumnWidth)
                    ]
                    [ diff
                        |> amountToMoneyLeftPad True (isExpanded |> either 0 maxIntegralDigits)
                        |> text
                    , viewIf isExpanded <| text " zł"
                    ]
                , viewIf isCollapsed <| viewTransactionTags ctx False tv
                ]

        personIdToName : Int -> String
        personIdToName pid =
            List.Extra.find (\p -> p.id == pid) ctx.model.people
                |> Maybe.andThen (Just << .name)
                |> Maybe.withDefault ""

        viewDetails () =
            column [ paddingXY 0 7, spacing 7 ]
                [ row [ width <| px transactionMoneyColumnWidth ]
                    [ text <| personIdToName t.payorId
                    , text <| " → "
                    , text <| String.join ", " <| List.map personIdToName t.beneficientIds
                    , Element.el [ Font.size 10 ] <| text <| " (" ++ String.fromFloat (amountToMoney t.amount) ++ " zł)"
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
        , width (px 300) |> attrWhen isExpanded
        , userSelectNone
        , inFront (Element.el [ alignRight, moveLeft 6, moveDown 4 ] <| viewTransactionTags ctx True tv) |> attrWhen isExpanded
        ]
        [ basics
        , if isExpanded then
            viewDetails ()

          else
            Element.none
        ]


viewTransactionTags : Context msg -> Bool -> TransactionView -> Element msg
viewTransactionTags ctx inColumn tv =
    (inColumn |> either column row)
        [ spacing 3 ]
        (List.map (\tag -> viewTag ctx tv tag) tv.transaction.tags)


viewTag : Context msg -> TransactionView -> String -> Element msg
viewTag ctx tv tag =
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
