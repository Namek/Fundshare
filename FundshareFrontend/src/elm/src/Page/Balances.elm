module Page.Balances exposing (Model, Msg, init, update, view)

import Cmd.Extra
import Data.Balance exposing (Balance)
import Data.Context exposing (..)
import Data.Person exposing (Person, PersonId)
import Data.Session exposing (Session)
import Element exposing (Element, FocusStyle, above, alignBottom, alignLeft, alignRight, alignTop, centerY, column, el, explain, fill, focused, height, inFront, maximum, mouseDown, moveDown, moveUp, padding, paddingEach, paragraph, px, rgb, rgb255, rgba, row, spaceEvenly, spacing, text, width, wrappedRow)
import Element.Background as Bg
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Html
import Html.Attributes
import Json.Decode as Json
import List.Extra
import Maybe.Extra
import Misc exposing (attrWhen, css, edges, either, grayed, noShadow, teal100, teal500, teal700, userSelectNone, viewBadge, viewIcon, viewIconButton, viewIf, white)
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


type alias BalanceCard =
    { title : String
    , preTitle : String
    , totalBalance : Float
    , sharedPaymentCount : Int
    , transferCount : Int
    , unseenUpdateCount : Int
    , lastUpdateAt : Maybe Posix
    , dateTimeNow : Maybe Posix
    , backgroundColor : Element.Color
    , interactMsg : InteractedBalanceCard
    }


type InteractedBalanceCard
    = InteractedTotalCard
    | InteractedPersonCard PersonId


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
    | NewTransaction InteractedBalanceCard
    | SeeEvents InteractedBalanceCard


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

        NewTransaction card ->
            -- TODO navigate
            ( model, Cmd.none )

        SeeEvents card ->
            -- TODO navigate
            ( model, Cmd.none )



-- VIEW --


view : Context msg -> Element msg
view ctx =
    let
        balances =
            ctx.model.balances

        extractCardData : Balance -> BalanceCard
        extractCardData balance =
            { title = balance.name
            , preTitle = "with"
            , totalBalance = balance.value
            , sharedPaymentCount = balance.sharedPaymentCount
            , transferCount = balance.transferCount
            , unseenUpdateCount = balance.unseenUpdateCount
            , lastUpdateAt = balance.lastUpdateAt
            , dateTimeNow = ctx.model.dateTimeNow
            , backgroundColor = teal700
            , interactMsg = InteractedPersonCard balance.personId
            }
    in
    wrappedRow
        []
        ((viewBalanceSummaryCard ctx
            :: (balances
                    |> List.map (extractCardData >> viewBalanceCard ctx)
               )
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

        sharedPaymentCount =
            balances |> List.map .sharedPaymentCount |> List.sum

        transferCount =
            balances |> List.map .transferCount |> List.sum

        unseenUpdateCount =
            balances |> List.map .unseenUpdateCount |> List.sum

        lastUpdateAt =
            balances
                |> List.filterMap .lastUpdateAt
                |> List.Extra.maximumBy Time.posixToMillis

        cardData =
            { title = "Total"
            , preTitle = ""
            , totalBalance = totalBalance
            , sharedPaymentCount = sharedPaymentCount
            , transferCount = transferCount
            , unseenUpdateCount = unseenUpdateCount
            , lastUpdateAt = lastUpdateAt
            , dateTimeNow = ctx.model.dateTimeNow
            , backgroundColor = teal500
            , interactMsg = InteractedTotalCard
            }
    in
    viewBalanceCard ctx cardData


viewBalanceCard : Context msg -> BalanceCard -> Element msg
viewBalanceCard ctx cardData =
    let
        iHaveMore =
            cardData.totalBalance > 0

        valueSign =
            iHaveMore |> either "+" "-"

        comparedColor =
            if cardData.totalBalance == 0 then
                Font.color (rgb255 255 255 255)

            else
                iHaveMore
                    |> either
                        (Font.color (rgb255 255 255 255))
                        (Font.color (rgb255 255 245 157))

        lastChangeDatetime =
            case ( cardData.lastUpdateAt, cardData.dateTimeNow ) of
                ( Just last, Just now ) ->
                    timeDistanceInWords True last now ++ " ago"

                _ ->
                    "no changes yet"
    in
    column
        [ width (px 340)
        , Bg.color cardData.backgroundColor
        , Border.shadow { offset = ( 3, 3 ), size = 1, blur = 20, color = rgba 0 0 0 0.2 }
        , Border.rounded 2
        , userSelectNone
        ]
        [ row [ width fill, spaceEvenly, padding 15 ]
            [ column [ spacing 15, paddingEach { edges | bottom = 4 } ]
                [ row
                    [ Font.color white
                    , Font.size 22
                    , css "align-items" "baseline"
                    , spacing 5
                    ]
                    [ el [ css "opacity" "0.8", css "font-size" "0.8em" ] <|
                        text cardData.preTitle
                    , el [] <| text cardData.title
                    ]
                , column [ Font.color grayed, Font.size 14, spacing 6 ]
                    [ el [] <| text (String.fromInt cardData.sharedPaymentCount ++ " shared payments")
                    , el [] <| text (String.fromInt cardData.transferCount ++ " transfers")
                    ]
                ]
            , row
                [ alignRight, alignBottom, spacing 4 ]
                [ viewIf (cardData.totalBalance /= 0)
                    (paragraph
                        [ comparedColor, centerY, Font.size 22 ]
                        [ text (valueSign ++ " ") ]
                    )
                , paragraph
                    [ Font.color white, Font.size 36, alignBottom, moveDown 2 ]
                    [ text <| String.fromFloat <| abs cardData.totalBalance ]
                , paragraph
                    [ Font.color grayed, Font.size 18, alignBottom ]
                    [ text "zł" ]
                ]
            ]

        {- horizontal line -}
        , el [ width fill, height (px 1), Bg.color (rgb255 0x00 0x82 0x76) ] Element.none
        , row
            [ width fill
            , spaceEvenly
            , centerY
            , padding 15
            ]
            [ el
                [ Font.size 12
                , Font.color (rgb255 0x89 0xCC 0xC6)
                , alignLeft
                ]
                (text lastChangeDatetime)
            , row [ alignRight, spacing 5 ]
                [ viewIconButton "plus"
                    (ctx.lift <| NewTransaction cardData.interactMsg)
                    []
                , viewIconButton "calendar"
                    (ctx.lift <| SeeEvents cardData.interactMsg)
                    [ attrWhen (cardData.unseenUpdateCount > 0) <|
                        inFront <|
                            el [ paddingEach { edges | left = 15 }, moveUp 8 ] <|
                                (viewBadge <| String.fromInt <| cardData.unseenUpdateCount)
                    ]
                ]
            ]
        ]
