module Views.Page exposing (ActivePage(..), accent, frame, primary)

import Data.Context exposing (GlobalMsg(..))
import Data.Session exposing (SessionState(..))
import Data.Transaction exposing (TransactionId)
import Data.User exposing (..)
import Html exposing (Html, a, div, footer, i, li, nav, span, text, ul)
import Html.Lazy exposing (lazy)
import List.Extra
import Material
import Material.Color as Color
import Material.Icon as Icon
import Material.Layout as Layout
import Material.Options as Options exposing (css)
import Material.Scheme
import Route exposing (Route, routeToString)
import Styles.Common exposing (commonGlobalStylesheet)
import Views.Spinner exposing (spinner)


{-| Determines which navbar link (if any) will be rendered as active.

Note that we don't enumerate every page here, because the navbar doesn't
have links for every page. Anything that's not part of the navbar falls
under Other.

-}
type ActivePage
    = Other
    | Login
    | Register
    | Settings
    | Profile UserId
    | NewTransaction
    | Balances
    | Transaction TransactionId
    | TransactionList


isActive : ActivePage -> Route -> Bool
isActive page route =
    case ( page, route ) of
        ( Login, Route.Login ) ->
            True

        -- ( Register, Route.Register ) ->
        --     True
        -- ( Profile pageUsername, Route.Profile routeUsername ) ->
        --     pageUsername == routeUsername
        ( NewTransaction, Route.NewTransaction ) ->
            True

        ( Balances, Route.Balances ) ->
            True

        ( Transaction pagePaymentId, Route.Transaction routePaymentId ) ->
            pagePaymentId == routePaymentId

        ( TransactionList, Route.TransactionList ) ->
            True

        _ ->
            False


tabData isLoggedIn =
    if isLoggedIn then
        [ ( "Balances", Balances, Route.Balances, "trending_up" )
        , ( "History", TransactionList, Route.TransactionList, "event" )
        , ( "New Transaction", NewTransaction, Route.NewTransaction, "add" )
        ]
    else
        []


tabs : Bool -> List (Html msg)
tabs isLoggedIn =
    tabData isLoggedIn
        |> List.map
            (\( str, page, route, icon ) ->
                Options.div
                    [ css "display" "flex"
                    , css "height" "100%"
                    , css "align-items" "center"
                    ]
                    [ Icon.view icon [ css "font-size" "28px" ] ]
            )


drawerData =
    [ ( "Balances", Balances, Route.Balances, "trending_up" )
    , ( "History", TransactionList, Route.TransactionList, "event" )
    , ( "New Transaction", NewTransaction, Route.NewTransaction, "add" )
    , ( "Log out", Other, Route.Logout, "exit_to_app" )
    ]


drawer : SessionState -> (GlobalMsg -> msg) -> (Material.Msg m -> msg) -> List (Html msg)
drawer session lift liftMdl =
    case session of
        LoggedSession session ->
            [ Layout.title [] [ text ("Hi, " ++ session.user.name) ]
            , Layout.navigation
                []
                (drawerData
                    |> List.map
                        (\( str, page, route, icon ) ->
                            Layout.link
                                [ Layout.href (routeToString route)
                                , Options.onClick (Layout.toggleDrawer liftMdl)
                                , css "display" "flex"
                                , css "align-content" "center"
                                ]
                                [ Icon.i icon
                                , Options.span [ css "margin-left" "15px" ] [ text str ]
                                ]
                        )
                )
            ]

        GuestSession ->
            []


getNthTabRoute : Bool -> Int -> Route
getNthTabRoute isLoggedIn index =
    List.Extra.getAt index (tabData isLoggedIn)
        |> Maybe.andThen (\( _, _, route, _ ) -> Just route)
        |> Maybe.withDefault Route.Login


findTabIndex : Bool -> ActivePage -> Int
findTabIndex isLoggedIn activePage =
    let
        rec tabs i =
            case tabs of
                ( _, x, _, _ ) :: xs ->
                    if x == activePage then
                        i
                    else
                        rec xs (i + 1)

                [] ->
                    -1
    in
    rec (tabData isLoggedIn) 0


primary : Color.Hue
primary =
    Color.Blue


accent : Color.Hue
accent =
    Color.Red


{-| Take a page's Html and frame it with a header and footer.

isLoading is for determining whether we should show a loading spinner
in the header. (This comes up during slow page transitions.)

-}
frame : (GlobalMsg -> msg) -> Material.Model -> (Material.Msg m -> msg) -> Bool -> Bool -> SessionState -> ActivePage -> Html msg -> Html msg
frame lift mdl liftMdl isLoading isLoggedIn session page content =
    let
        c =
            Options.div [ css "width" "100%" ] <|
                [ lazy
                    (\isLoading ->
                        if isLoading then
                            spinner
                        else
                            content
                    )
                    isLoading
                ]
    in
    Material.Scheme.topWithScheme primary accent <|
        Layout.render liftMdl
            mdl
            [ Layout.seamed
            , Layout.fixedHeader
            , Layout.waterfall True
            , Layout.selectedTab (findTabIndex isLoggedIn page)
            , Layout.onSelectTab (getNthTabRoute isLoggedIn >> Navigate >> lift)
            ]
            { header = viewHeader
            , drawer = drawer session lift liftMdl
            , main = [ viewMain c ]
            , tabs =
                ( tabs isLoggedIn
                , [ Color.background (Color.color primary Color.S400) ]
                )
            }


viewHeader : List (Html msg)
viewHeader =
    [ commonGlobalStylesheet |> Options.stylesheet
    , Layout.row []
        [ Layout.title [] [ text "Fund$hare" ]
        , Layout.spacer
        , Layout.navigation [] [ Layout.link [ Layout.href githubUrl ] [ span [] [ text "GitHub" ] ] ]
        ]
    ]


viewMain : Html msg -> Html msg
viewMain content =
    Options.div
        [ css "margin" "10px auto 0 auto"
        , css "display" "flex"
        , css "justify-content" "center"
        , css "padding" "0 5px"
        ]
        [ content ]


githubUrl =
    "https://github.com/Namek/fundshare"
