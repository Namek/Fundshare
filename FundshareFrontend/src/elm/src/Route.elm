module Route exposing (Route(..), fromLocation, href, modifyUrl, routeToString)

import Data.Transaction exposing (TransactionId)
import Data.User as User exposing (UserId)
import Html exposing (Attribute)
import Html.Attributes as Attr
import Navigation exposing (Location)
import UrlParser as Url exposing ((</>), Parser, oneOf, parseHash, s, string)


-- ROUTING --


type Route
    = Login
    | Logout
      -- | Register
      -- | Profile UserId
    | NewTransaction
    | Balances
    | Transaction TransactionId
    | TransactionList


route : Parser (Route -> a) a
route =
    oneOf
        [ Url.map Login (s "login")
        , Url.map Logout (s "logout")

        -- , Url.map Register (s "register")
        -- , Url.map Profile (s "profile" </> User.usernameParser)
        , Url.map NewTransaction (s "pay")
        , Url.map Balances (s "balances")
        , Url.map Transaction (s "transaction" </> Url.int)
        , Url.map TransactionList (s "history")
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
                    [ "transaction", toString id ]

                TransactionList ->
                    [ "history" ]
    in
    "#/" ++ String.join "/" pieces



-- PUBLIC HELPERS --


href : Route -> Attribute msg
href route =
    Attr.href (routeToString route)


modifyUrl : Route -> Cmd msg
modifyUrl =
    routeToString >> Navigation.modifyUrl


fromLocation : Location -> Maybe Route
fromLocation location =
    if String.isEmpty location.hash then
        Just NewTransaction
    else
        parseHash route location
