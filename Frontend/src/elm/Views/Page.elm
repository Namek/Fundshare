module Views.Page exposing (ActivePage(..), Page(..), frame)

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
import Page.Login as Login
import Page.Mailbox as Inbox
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
    | Mailbox Inbox.Model
    | TransactionHistory TransactionHistory.Model


type ActivePage
    = Route_Unknown
    | Route_Balances
    | Route_Mailbox
    | Route_History
    | Route_NewTransaction


frame : (GlobalMsg -> msg) -> Bool -> SessionState -> ActivePage -> Element msg -> Element msg
frame lift isLoggedIn session activePage pageContent =
    let
        content =
            column [ width fill, spacing 10 ]
                [ viewIf isLoggedIn <| Element.el [ centerX ] <| viewMenu session activePage
                , Element.el [ centerX ] pageContent
                ]
    in
    Element.el
        [ width fill ]
        content


viewMenu : SessionState -> ActivePage -> Element msg
viewMenu sessionState activePage =
    let
        alink : Maybe ActivePage -> Route -> String -> Element msg
        alink page route label =
            let
                isActivePage =
                    page
                        |> Maybe.andThen ((==) activePage >> Just)
                        |> Maybe.withDefault False
            in
            link
                [ Font.underline |> attrWhen isActivePage
                ]
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
                    [ alink (Just Route_Balances) Route.Balances "Balances"
                    , Element.el
                        [ onRight
                            (el [ moveLeft 5, moveUp 8 ] <|
                                (viewBadge <| String.fromInt <| session.inboxSize)
                            )
                            |> attrWhen (session.inboxSize > 0)
                        ]
                        (alink (Just Route_Mailbox) Route.Mailbox "Mail")
                    , alink (Just Route_History) (Route.TransactionHistory Nothing) "History"
                    , alink (Just Route_NewTransaction) Route.NewTransaction "Add transaction"
                    , alink Nothing Route.Logout "Logout"
                    ]
                ]

            _ ->
                []
        )
