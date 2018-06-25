module Main exposing (init, subscriptions, update)

import Data.Context exposing (GlobalMsg(..))
import Data.Session as Session exposing (Session, SessionState(..))
import Data.Transaction exposing (TransactionId)
import Data.User exposing (UserId(..))
import Html exposing (..)
import Json.Decode as Decode exposing (Value)
import Material exposing (subscriptions)
import Misc exposing ((=>))
import Navigation exposing (Location)
import Page.Balances as Balances
import Page.Errored as Errored exposing (PageLoadError(..))
import Page.Login as Login
import Page.NewTransaction as NewTransaction
import Page.NotFound as NotFound
import Page.Transaction as Transaction
import Page.TransactionList as TransactionList
import Ports
import Route exposing (Route, modifyUrl)
import Task
import Views.Page as Page exposing (ActivePage(..), frame)


-- STATE --


type alias Model =
    { pageState : PageState
    , session : SessionState
    , mdl : Material.Model
    }


init : Value -> Location -> ( Model, Cmd Msg )
init json location =
    setRoute (Route.fromLocation location)
        { pageState = Loaded initialPage
        , session = Session.decodeFromJson json
        , mdl = Material.model
        }


initialPage : Page
initialPage =
    Blank



-- VIEW --


view : Model -> Html Msg
view model =
    let
        context =
            { mdl = model.mdl
            , liftMaterial = MaterialMsg
            , model = ()
            , lift = ()
            , session = model.session
            }
    in
    case model.pageState of
        Loaded page ->
            viewPage model False page

        TransitioningFrom page ->
            viewPage model True page


viewPage : Model -> Bool -> Page -> Html Msg
viewPage model isLoading page =
    let
        context =
            { mdl = model.mdl
            , liftMaterial = MaterialMsg
            , model = ()
            , lift = ()
            , matId = List.append [ 0 ]
            , session = model.session
            }

        guestContext =
            { mdl = model.mdl
            , liftMaterial = MaterialMsg
            , model = ()
            , lift = ()
            , matId = List.append [ 0 ]
            }

        isLoggedIn =
            model.session /= GuestSession

        frame =
            Page.frame HandleGlobalMsg context.mdl context.liftMaterial isLoading isLoggedIn model.session

        getAuthorizedSession =
            \() ->
                case model.session of
                    LoggedSession session ->
                        session

                    _ ->
                        Debug.crash "you have to be authorized!"
    in
    case page of
        Blank ->
            frame Page.Other (Html.text "")

        NotFound ->
            NotFound.view model.session
                |> frame Page.Other

        Errored subModel ->
            Errored.view model.session subModel
                |> frame Page.Other

        Login subModel ->
            Login.view { guestContext | model = subModel, lift = LoginMsg }
                |> frame Page.Login

        NewTransaction subModel ->
            NewTransaction.view
                { context
                    | model = subModel
                    , lift = NewTransactionMsg
                    , session = getAuthorizedSession ()
                }
                |> frame Page.NewTransaction

        Balances subModel ->
            Balances.view
                { context
                    | model = subModel
                    , lift = BalancesMsg
                    , session = getAuthorizedSession ()
                }
                |> frame Page.Balances

        Transaction paymentId subModel ->
            Transaction.view
                { context
                    | model = subModel
                    , lift = TransactionMsg
                    , session = getAuthorizedSession ()
                }
                |> frame (Page.Transaction paymentId)

        TransactionList subModel ->
            TransactionList.view
                { context
                    | model = subModel
                    , lift = TransactionListMsg
                    , session = getAuthorizedSession ()
                }
                |> frame Page.TransactionList



-- SUBSCRIPTIONS --


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        []



-- UPDATE --


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


type Msg
    = SetRoute (Maybe Route)
    | MaterialMsg (Material.Msg Msg)
    | HandleGlobalMsg GlobalMsg
    | LoginLoaded (Result PageLoadError Login.Model)
    | LoginMsg Login.Msg
    | NewPaymentLoaded (Result PageLoadError NewTransaction.Model)
    | NewTransactionMsg NewTransaction.Msg
    | BalancesLoaded (Result PageLoadError Balances.Model)
    | BalancesMsg Balances.Msg
    | TransactionMsg Transaction.Msg
    | TransactionListMsg TransactionList.Msg


pageErrored : Model -> ActivePage -> String -> ( Model, Cmd msg )
pageErrored model activePage errorMessage =
    let
        error =
            Errored.pageLoadError activePage errorMessage
    in
    { model | pageState = Loaded (Errored error) } ! []


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        MaterialMsg msg ->
            Material.update MaterialMsg msg model

        _ ->
            updatePage (getPage model.pageState) msg model


updatePage : Page -> Msg -> Model -> ( Model, Cmd Msg )
updatePage page msg model =
    let
        session =
            model.session

        context =
            { mdl = model.mdl
            , liftMaterial = MaterialMsg
            , model = ()
            , lift = ()
            , matId = List.append [ 0 ]
            , session = model.session
            }

        guestContext =
            { mdl = model.mdl
            , liftMaterial = MaterialMsg
            , model = ()
            , lift = ()
            , matId = List.append [ 0 ]
            }

        toPage toModel toMsg subUpdate subMsg =
            let
                ( newModel, newCmd ) =
                    subUpdate subMsg
            in
            ( { model | pageState = Loaded (toModel newModel) }, Cmd.map toMsg newCmd )

        toPageWithGlobalMsgs toModel toMsg subUpdate subMsg =
            let
                ( ( pageModel, cmd ), globalMsg ) =
                    subUpdate subMsg
            in
            { model | pageState = Loaded (toModel pageModel) }
                => Cmd.batch
                    [ Cmd.map toMsg cmd
                    , Cmd.map HandleGlobalMsg globalMsg
                    ]

        getSession =
            \() ->
                case model.session of
                    LoggedSession session ->
                        session

                    _ ->
                        Debug.crash "you are not authorized!"

        errored =
            pageErrored model
    in
    case ( msg, page ) of
        ( SetRoute route, _ ) ->
            setRoute route model

        ( HandleGlobalMsg globalMsg, _ ) ->
            case globalMsg of
                Navigate route ->
                    model => Route.modifyUrl route

                SetSession session ->
                    { model | session = LoggedSession session } => Cmd.none

        ( LoginLoaded (Ok subModel), _ ) ->
            { model | pageState = Loaded (Login subModel) } => Cmd.none

        ( LoginMsg subMsg, Login subModel ) ->
            let
                ctx =
                    { guestContext
                        | model = subModel
                        , lift = LoginMsg
                    }

                ( ( pageModel, cmd ), globalMsg ) =
                    Login.update ctx subMsg
            in
            { model | pageState = Loaded (Login pageModel) }
                => Cmd.batch
                    [ Cmd.map LoginMsg cmd
                    , Cmd.map HandleGlobalMsg globalMsg
                    ]

        ( NewPaymentLoaded (Ok subModel), _ ) ->
            { model | pageState = Loaded (NewTransaction subModel) } ! []

        ( NewTransactionMsg subMsg, NewTransaction subModel ) ->
            let
                ctx =
                    { context
                        | model = subModel
                        , lift = NewTransactionMsg
                        , session = getSession ()
                    }

                ( ( pageModel, cmd ), globalMsg ) =
                    NewTransaction.update ctx subMsg
            in
            { model | pageState = Loaded (NewTransaction pageModel) }
                => Cmd.batch
                    [ Cmd.map NewTransactionMsg cmd
                    , Cmd.map HandleGlobalMsg globalMsg
                    ]

        ( BalancesLoaded (Ok subModel), _ ) ->
            { model | pageState = Loaded (Balances subModel) } ! []

        ( BalancesMsg subMsg, Balances subModel ) ->
            let
                ctx =
                    { context
                        | model = subModel
                        , lift = BalancesMsg
                        , session = getSession ()
                    }
            in
            toPage Balances BalancesMsg (Balances.update ctx) subMsg

        ( TransactionMsg subMsg, Transaction paymentId subModel ) ->
            let
                ctx =
                    { context
                        | model = subModel
                        , lift = TransactionMsg
                        , session = getSession ()
                    }
            in
            toPageWithGlobalMsgs (Transaction paymentId) TransactionMsg (Transaction.update ctx) subMsg

        ( TransactionListMsg subMsg, TransactionList subModel ) ->
            let
                ctx =
                    { context
                        | model = subModel
                        , lift = TransactionListMsg
                        , session = getSession ()
                    }
            in
            toPageWithGlobalMsgs TransactionList TransactionListMsg (TransactionList.update ctx) subMsg

        ( _, NotFound ) ->
            -- Disregard incoming messages when we're on the
            -- NotFound page.
            model ! []

        ( _, _ ) ->
            -- Disregard incoming messages that arrived for the wrong page
            model ! []



-- HELPERS --


setRoute : Maybe Route -> Model -> ( Model, Cmd Msg )
setRoute maybeRoute model =
    let
        transition toMsg task =
            { model | pageState = TransitioningFrom (getPage model.pageState) }
                ! [ Task.attempt toMsg task ]

        errored =
            pageErrored model

        whenLogged func =
            case model.session of
                LoggedSession session ->
                    func session

                GuestSession ->
                    setRoute (Just Route.Login) model
    in
    case maybeRoute of
        Nothing ->
            { model | pageState = Loaded NotFound } ! []

        Just Route.Login ->
            { model | pageState = Loaded (Login Login.initialModel) } ! []

        Just Route.Logout ->
            ( { model | session = GuestSession }
            , Cmd.batch
                [ Ports.storeSession Nothing
                , Route.modifyUrl Route.Login
                ]
            )

        -- Just Route.Register ->
        -- { model | pageState = Loaded (Register Register.initialModel) } ! []
        Just Route.NewTransaction ->
            case model.session of
                LoggedSession session ->
                    let
                        ( subModel, subMsg ) =
                            NewTransaction.init session
                    in
                    { model | pageState = Loaded (NewTransaction <| subModel) }
                        ! [ Cmd.map NewTransactionMsg subMsg ]

                GuestSession ->
                    setRoute (Just Route.Login) model

        Just Route.Balances ->
            whenLogged
                (\session ->
                    let
                        ( subModel, subMsg ) =
                            Balances.init session
                    in
                    { model | pageState = Loaded (Balances <| subModel) }
                        ! [ Cmd.map BalancesMsg subMsg ]
                )

        Just (Route.Transaction paymentId) ->
            whenLogged
                (\session ->
                    let
                        ( subModel, subMsg ) =
                            Transaction.init session paymentId
                    in
                    { model | pageState = Loaded (Transaction paymentId <| subModel) }
                        ! [ Cmd.map TransactionMsg subMsg ]
                )

        Just Route.TransactionList ->
            whenLogged
                (\session ->
                    let
                        ( subModel, subMsg ) =
                            TransactionList.init session
                    in
                    { model | pageState = Loaded (TransactionList <| subModel) }
                        ! [ Cmd.map TransactionListMsg subMsg ]
                )


getPage : PageState -> Page
getPage pageState =
    case pageState of
        Loaded page ->
            page

        TransitioningFrom page ->
            page



-- MAIN --


main : Program Value Model Msg
main =
    Navigation.programWithFlags (Route.fromLocation >> SetRoute)
        { init = init
        , view = view
        , subscriptions = subscriptions
        , update = update
        }
