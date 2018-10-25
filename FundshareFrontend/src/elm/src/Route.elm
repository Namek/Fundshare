module Route exposing (Route(..), fromUrl, href, modifyUrl, routeToString)

import Browser.Navigation as Nav
import Data.Transaction exposing (TransactionId)
import Html exposing (Attribute)
import Html.Attributes as Attr
import Url exposing (Url)
import Url.Parser as UrlParser exposing ((</>), Parser, int, map, oneOf, s)



-- ROUTING --


type Route
    = Login
    | Logout
      -- | Register
      -- | Profile UserId
    | NewTransaction
    | Balances
    | Transaction TransactionId
    | Inbox
    | TransactionHistory


parseRoute : Parser (Route -> a) a
parseRoute =
    oneOf
        [ map Login (s "login")
        , map Logout (s "logout")

        -- , Url.map Register (s "register")
        -- , Url.map Profile (s "profile" </> User.usernameParser)
        , map NewTransaction (s "pay")
        , map Balances (s "balances")
        , map Transaction (s "transaction" </> int)
        , map Inbox (s "inbox")
        , map TransactionHistory (s "history")
        ]



-- INTERNAL --


routeToString : Route -> String
routeToString page =
    let
        pieces =
            case page of
                Login ->
                    [ "login" ]

                Logout ->
                    [ "logout" ]

                -- Register ->
                -- [ "register" ]
                -- Profile username ->
                --     [ "profile", User.usernameToString username ]
                NewTransaction ->
                    [ "pay" ]

                Balances ->
                    [ "balances" ]

                Transaction id ->
                    [ "transaction", String.fromInt id ]

                Inbox ->
                    [ "inbox" ]

                TransactionHistory ->
                    [ "history" ]
    in
    "#/" ++ String.join "/" pieces



-- PUBLIC HELPERS --


href : Route -> Attribute msg
href route =
    Attr.href (routeToString route)


modifyUrl : { a | navKey : Nav.Key } -> Route -> Cmd msg
modifyUrl state route =
    routeToString route |> Nav.pushUrl state.navKey


fromUrl : Url -> Maybe Route
fromUrl url =
    { url | path = Maybe.withDefault "" url.fragment, fragment = Nothing }
        |> UrlParser.parse parseRoute
