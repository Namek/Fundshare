module Views.Page exposing (Page(..), PageState(..), frame)

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
import Page.Login as Login
import Page.NewTransaction as NewTransaction
import Page.NotFound as NotFound
import Page.Transaction as Transaction
import Page.TransactionList as TransactionList
import Route exposing (Route, routeToString)
import Styles.Common exposing (commonGlobalStylesheet)
import Views.Spinner exposing (spinner)


type Page
    = Blank
    | NotFound
    | Errored PageLoadError
    | Login Login.Model
    | NewTransaction NewTransaction.Model
    | Balances Balances.Model
    | Transaction TransactionId Transaction.Model
    | TransactionList TransactionList.Model


type PageState
    = Loaded Page
    | TransitioningFrom Page



-- tabData isLoggedIn =
--     if isLoggedIn then
--         [ ( "Balances", Balances, Route.Balances, "trending_up" )
--         , ( "History", TransactionList, Route.TransactionList, "event" )
--         , ( "New Transaction", NewTransaction, Route.NewTransaction, "add" )
--         ]
--
--     else
--         []
--
--
-- drawerData =
--     [ ( "Balances", Balances, Route.Balances, "trending_up" )
--     , ( "History", TransactionList, Route.TransactionList, "event" )
--     , ( "New Transaction", NewTransaction, Route.NewTransaction, "add" )
--     , ( "Log out", Other, Route.Logout, "exit_to_app" )
--     ]


{-| Take a page's Html and frame it with a header and footer.

isLoading is for determining whether we should show a loading spinner
in the header. (This comes up during slow page transitions.)

-}
frame : (GlobalMsg -> msg) -> Bool -> Bool -> SessionState -> Element msg -> Element msg
frame lift isLoading isLoggedIn session pageContent =
    let
        content =
            if isLoading then
                Element.el [] (text "omg!!11")

            else
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
                , alink Route.TransactionList "Inbox"
                , alink Route.NewTransaction "Add transaction"
                , alink Route.Logout "Logout"
                ]
            ]

         else
            []
        )



{- TODO some tabs here? -}
{- viewHeader, viewMain -}
-- Material.Scheme.topWithScheme primary accent <|
--     Layout.render liftMdl
--         mdl
--         [ Layout.seamed
--         , Layout.fixedHeader
--         , Layout.waterfall True
--         , Layout.selectedTab (findTabIndex isLoggedIn page)
--         , Layout.onSelectTab (getNthTabRoute isLoggedIn >> Navigate >> lift)
--         ]
--         { header = viewHeader
--         , drawer = drawer session lift liftMdl
--         , main = [ viewMain c ]
--         , tabs =
--             ( tabs isLoggedIn
--             , [ Color.background (Color.color primary Color.S400) ]
--             )
--         }
-- viewHeader : List (Element msg)
-- viewHeader =
-- [ commonGlobalStylesheet |> Options.stylesheet
-- , Layout.row []
--     [ Layout.title [] [ text "Fund$hare" ]
--     , Layout.spacer
--     , Layout.navigation [] [ Layout.link [ Layout.href githubUrl ] [ span [] [ text "GitHub" ] ] ]
--     ]
-- ]
-- viewMain : Element msg -> Html msg
-- viewMain content =
--     Element.layout
--         [ paddingEach { edges | top = 10, left = 5, right = 5 }
--         , centerX
--         , centerY
--         ]
--         content
