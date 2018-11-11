module Views.Page exposing (Page(..), frame)

import Data.Context exposing (GlobalMsg(..))
import Data.Session exposing (SessionState(..))
import Data.Transaction exposing (TransactionId)
import Element exposing (Element, centerX, centerY, column, el, fill, inFront, link, moveLeft, moveUp, newTabLink, onRight, padding, paddingEach, paddingXY, row, shrink, spacing, text, width, wrappedRow)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Lazy
import Html exposing (Html)
import List.Extra
import Misc exposing (attrWhen, defaultShadow, edges, either, viewBadge, viewIf)
import Misc.Colors as Colors
import Page.Balances as Balances
import Page.Errored as Errored exposing (PageLoadError(..))
import Page.Inbox as Inbox
import Page.Login as Login
import Page.NewTransaction as NewTransaction
import Page.NotFound as NotFound
import Page.Transaction as Transaction
import Page.TransactionHistory as TransactionHistory
import Route exposing (Route, routeToString)
import Views.Spinner exposing (spinner)


type Page
    = Blank
    | NotFound
    | Errored PageLoadError
    | Login Login.Model
    | NewTransaction NewTransaction.Model
    | Balances Balances.Model
    | Transaction TransactionId Transaction.Model
    | Inbox Inbox.Model
    | TransactionHistory TransactionHistory.Model


frame : (GlobalMsg -> msg) -> Bool -> SessionState -> Element msg -> Element msg
frame lift isLoggedIn session pageContent =
    let
        content =
            viewIf isLoggedIn <|
                column [ width fill, spacing 10 ]
                    [ Element.el [ centerX ] <| viewMenu session
                    , Element.el [ centerX ] pageContent
                    ]
    in
    Element.el
        [ width fill ]
        content


viewMenu : SessionState -> Element msg
viewMenu sessionState =
    let
        alink route label =
            link []
                { url = routeToString route
                , label = text label
                }
    in
    row
        [ width shrink
        , paddingXY 20 10
        , Background.color Colors.teal500
        , Font.color Colors.white
        , Font.size 15
        , Border.rounded 2
        , defaultShadow
        ]
        (case sessionState of
            LoggedSession session ->
                [ row [ centerX, spacing 18 ]
                    [ alink Route.Balances "Balances"
                    , Element.el
                        [ onRight
                            (el [ moveLeft 5, moveUp 8 ] <|
                                (viewBadge <| String.fromInt <| session.inboxSize)
                            )
                            |> attrWhen (session.inboxSize > 0)
                        ]
                        (alink Route.Inbox "Inbox")
                    , alink (Route.TransactionHistory Nothing) "History"
                    , alink Route.NewTransaction "Add transaction"
                    , alink Route.Logout "Logout"
                    ]
                ]

            _ ->
                []
        )
