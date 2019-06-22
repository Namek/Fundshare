module Route exposing (Route(..), fromUrl, href, modifyUrl, routeToString)

import Browser.Navigation as Nav
import Data.Transaction exposing (TransactionId)
import Html exposing (Attribute)
import Html.Attributes as Attr
import Url exposing (Url)
import Url.Parser as UrlParser exposing ((</>), (<?>), Parser, int, map, oneOf, s)
import Url.Parser.Query as Query



-- ROUTING --


type Route
    = Login
    | Logout
    | NewTransaction
    | Balances
    | Transaction TransactionId
    | Inbox
    | TransactionHistory (Maybe Int)


parseRoute : Parser (Route -> a) a
parseRoute =
    oneOf
        [ map Login (s "login")
        , map Logout (s "logout")
        , map NewTransaction (s "pay")
        , map Balances (s "balances")
        , map Transaction (s "transaction" </> int)
        , map Inbox (s "inbox")
        , map TransactionHistory (s "history" <?> Query.int "page")
        ]


routeToString : Route -> String
routeToString page =
    let
        url =
            case page of
                Login ->
                    "login"

                Logout ->
                    "logout"

                NewTransaction ->
                    "pay"

                Balances ->
                    "balances"

                Transaction id ->
                    "transaction/" ++ String.fromInt id

                Inbox ->
                    "inbox"

                TransactionHistory Nothing ->
                    "history"

                TransactionHistory (Just pageNo) ->
                    "history/?page=" ++ String.fromInt pageNo
    in
    "#/" ++ url


href : Route -> Attribute msg
href route =
    Attr.href (routeToString route)


modifyUrl : { a | navKey : Nav.Key } -> Route -> Cmd msg
modifyUrl state route =
    routeToString route |> Nav.pushUrl state.navKey


{-| URIs in this program are written with # in the beginning
so we have to move URI parts so parser can handle it.
-}
fromUrl : Url -> Maybe Route
fromUrl url =
    let
        splitted =
            url.fragment
                |> Maybe.andThen (String.split "?" >> Just)
                |> Maybe.withDefault []

        path =
            List.head splitted
                |> Maybe.withDefault ""

        query =
            List.drop 1 splitted
                |> List.head
    in
    { url
        | path = path
        , fragment = Nothing
        , query = query
    }
        |> UrlParser.parse parseRoute
