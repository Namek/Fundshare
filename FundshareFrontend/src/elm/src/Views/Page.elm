module Views.Page exposing (Page(..), frame)

import Data.Context exposing (GlobalMsg(..))
import Data.Session exposing (SessionState(..))
import Data.Transaction exposing (TransactionId)
import Data.User exposing (..)
import Element exposing (Element, centerX, centerY, column, el, fill, link, newTabLink, padding, paddingEach, row, shrink, spacing, text, width, wrappedRow)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Lazy
import Html exposing (Html)
import List.Extra
import Misc exposing (defaultShadow, edges, either)
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
            column [ width fill, spacing 10 ]
                [ viewMenu isLoggedIn
                , Element.el [ centerX ] pageContent
                ]
    in
    Element.el
        [ width fill
        , paddingEach { edges | left = 5, right = 5, top = 5, bottom = 5 }
        ]
        content


viewMenu isLoggedIn =
    let
        alink route label =
            link []
                { url = routeToString route
                , label = text label
                }
    in
    row
        [ width fill
        , padding 10
        , Background.color Colors.blueGray50
        , Font.size 15
        , Border.rounded 1
        , defaultShadow
        ]
        (if isLoggedIn then
            [ row [ centerX, spacing 18 ]
                [ alink Route.Balances "Balances"
                , alink Route.Inbox "Inbox"
                , alink (Route.TransactionHistory Nothing) "History"
                , alink Route.NewTransaction "Add transaction"
                , alink Route.Logout "Logout"
                ]
            ]

         else
            []
        )
