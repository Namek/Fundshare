module Main exposing (main)

import Browser exposing (Document)
import Browser.Navigation as Nav
import Cmd.Extra
import Data.Context exposing (GlobalMsg(..))
import Data.Session as Session exposing (Session, SessionState(..))
import Data.Transaction exposing (TransactionId)
import Data.User exposing (User, UserId(..))
import Element exposing (Element, paragraph, text)
import GraphQL.Client.Http
import Html exposing (Html)
import Json.Decode as Decode exposing (Value)
import Maybe.Extra
import Misc exposing (noCmd)
import Page.Balances as Balances
import Page.Errored as Errored exposing (PageLoadError(..))
import Page.Login as Login
import Page.NewTransaction as NewTransaction
import Page.NotFound as NotFound
import Page.Transaction as Transaction
import Page.TransactionList as TransactionList
import Request.Common exposing (sendMutationRequest)
import Request.Session exposing (SignInResult, checkSession)
import Route exposing (Route, modifyUrl)
import Task
import Url exposing (Url)
import Views.Page as Page exposing (Page(..), PageState(..), frame)



-- MAIN --


main : Program Value Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , subscriptions = subscriptions
        , update = update
        , onUrlRequest = UrlRequested
        , onUrlChange = UrlChanged
        }



-- STATE --


type alias Model =
    { navKey : Nav.Key
    , pageState : PageState
    , lastLocation : Url
    , session : SessionState
    }


init : Value -> Url -> Nav.Key -> ( Model, Cmd Msg )
init json url navKey =
    {- check authorization before setting location from URL -}
    let
        model =
            { navKey = navKey
            , pageState = Loaded initialPage
            , lastLocation = url
            , session = GuestSession
            }
    in
    ( model, Cmd.Extra.perform CheckAuthSession )


initialPage : Page
initialPage =
    Blank



-- VIEW --


view : Model -> Document Msg
view model =
    { title = "Fundshare"
    , body =
        [ Element.layout [] <|
            case model.pageState of
                Loaded page ->
                    viewPage model False page

                TransitioningFrom page ->
                    viewPage model True page
        ]
    }


viewPage : Model -> Bool -> Page -> Element Msg
viewPage model isLoading page =
    let
        isLoggedIn =
            model.session /= GuestSession

        frame =
            Page.frame HandleGlobalMsg isLoading isLoggedIn model.session

        getAuthorizedSession =
            \() ->
                case model.session of
                    LoggedSession session ->
                        session

                    _ ->
                        Debug.todo "you have to be authorized!"
    in
    case page of
        Blank ->
            paragraph [] [ text "" ]
                |> frame

        NotFound ->
            NotFound.view model.session
                |> frame

        Errored subModel ->
            Errored.view model.session subModel
                |> frame

        Login subModel ->
            Login.view { model = subModel, lift = LoginMsg }
                |> frame

        NewTransaction subModel ->
            NewTransaction.view
                { model = subModel
                , lift = NewTransactionMsg
                , session = getAuthorizedSession ()
                }
                |> frame

        Balances subModel ->
            Balances.view
                { model = subModel
                , lift = BalancesMsg
                , session = getAuthorizedSession ()
                }
                |> frame

        Transaction paymentId subModel ->
            Transaction.view
                { model = subModel
                , lift = TransactionMsg
                , session = getAuthorizedSession ()
                }
                |> frame

        TransactionList subModel ->
            TransactionList.view
                { model = subModel
                , lift = TransactionListMsg
                , session = getAuthorizedSession ()
                }
                |> frame



-- SUBSCRIPTIONS --


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        []



-- UPDATE --


type Msg
    = HandleGlobalMsg GlobalMsg
    | UrlChanged Url.Url
    | UrlRequested Browser.UrlRequest
    | SetRoute (Maybe Route)
    | CheckAuthSession
    | CheckAuthSessionResponse (Result GraphQL.Client.Http.Error (Maybe SignInResult))
    | LoginLoaded (Result PageLoadError Login.Model)
    | LoginMsg Login.Msg
    | NewPaymentLoaded (Result PageLoadError NewTransaction.Model)
    | NewTransactionMsg NewTransaction.Msg
    | BalancesLoaded (Result PageLoadError Balances.Model)
    | BalancesMsg Balances.Msg
    | TransactionMsg Transaction.Msg
    | TransactionListMsg TransactionList.Msg


pageErrored : Model -> String -> ( Model, Cmd msg )
pageErrored model errorMessage =
    let
        error =
            Errored.pageLoadError errorMessage
    in
    ( { model | pageState = Loaded (Errored error) }
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        HandleGlobalMsg globalMsg ->
            case globalMsg of
                Navigate route ->
                    ( model, Route.modifyUrl model route )

                SetSession (Just session) ->
                    ( { model | session = LoggedSession session }, Cmd.none )

                SetSession Nothing ->
                    ( { model | session = GuestSession }, Cmd.none )

        UrlChanged url ->
            ( { model | lastLocation = url }
            , Cmd.Extra.perform (Route.fromUrl (Debug.log "urlchanged" url) |> SetRoute)
            )

        UrlRequested req ->
            case req of
                Browser.Internal url ->
                    ( model
                    , Cmd.Extra.perform (Route.fromUrl url |> SetRoute)
                    )

                Browser.External href ->
                    ( model, Nav.load href )

        SetRoute route ->
            setRoute route model

        CheckAuthSession ->
            let
                cmd =
                    checkSession
                        |> sendMutationRequest
                        |> Task.attempt CheckAuthSessionResponse
            in
            ( model, cmd )

        {- first thing that comes from this app to backend - decide whether user is still logged in -}
        CheckAuthSessionResponse response ->
            let
                maybeUser : Maybe SignInResult
                maybeUser =
                    response
                        |> Result.toMaybe
                        |> Maybe.andThen (\resp -> resp)
            in
            case maybeUser of
                {- no valid token, guest session -}
                Nothing ->
                    ( { model | session = GuestSession }
                    , Route.modifyUrl model Route.Login
                    )

                Just user ->
                    let
                        modelWithSession =
                            { model | session = LoggedSession { user = user } }

                        newRoute =
                            Route.fromUrl model.lastLocation
                                |> Maybe.Extra.or (Just Route.Login)

                        ( newModel, rerouteCmd ) =
                            setRoute newRoute modelWithSession
                    in
                    ( newModel, rerouteCmd )

        _ ->
            updatePage (getPage model.pageState) msg model


updatePage : Page -> Msg -> Model -> ( Model, Cmd Msg )
updatePage page msg model =
    let
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

                cmds =
                    Cmd.batch
                        [ Cmd.map toMsg cmd
                        , Cmd.map HandleGlobalMsg globalMsg
                        ]
            in
            ( { model | pageState = Loaded (toModel pageModel) }, cmds )

        getSession =
            \() ->
                case model.session of
                    LoggedSession session ->
                        session

                    _ ->
                        Debug.todo "you are not authorized!"

        errored =
            pageErrored model
    in
    case ( msg, page ) of
        ( LoginLoaded (Ok subModel), _ ) ->
            ( { model | pageState = Loaded (Login subModel) }, Cmd.none )

        ( LoginMsg subMsg, Login subModel ) ->
            let
                ctx =
                    { model = subModel
                    , lift = LoginMsg
                    }

                ( ( pageModel, cmd ), globalMsg ) =
                    Login.update ctx subMsg

                cmds =
                    Cmd.batch
                        [ Cmd.map LoginMsg cmd
                        , Cmd.map HandleGlobalMsg globalMsg
                        ]
            in
            ( { model | pageState = Loaded (Login pageModel) }, cmds )

        ( NewPaymentLoaded (Ok subModel), _ ) ->
            ( { model | pageState = Loaded (NewTransaction subModel) }
            , Cmd.none
            )

        ( NewTransactionMsg subMsg, NewTransaction subModel ) ->
            let
                ctx =
                    { model = subModel
                    , lift = NewTransactionMsg
                    , session = getSession ()
                    }

                ( ( pageModel, cmd ), globalMsg ) =
                    NewTransaction.update ctx subMsg

                cmds =
                    Cmd.batch
                        [ Cmd.map NewTransactionMsg cmd
                        , Cmd.map HandleGlobalMsg globalMsg
                        ]
            in
            ( { model | pageState = Loaded (NewTransaction pageModel) }, cmds )

        ( BalancesLoaded (Ok subModel), _ ) ->
            ( { model | pageState = Loaded (Balances subModel) }
            , Cmd.none
            )

        ( BalancesMsg subMsg, Balances subModel ) ->
            let
                ctx =
                    { model = subModel
                    , lift = BalancesMsg
                    , session = getSession ()
                    }
            in
            toPage Balances BalancesMsg (Balances.update ctx) subMsg

        ( TransactionMsg subMsg, Transaction paymentId subModel ) ->
            let
                ctx =
                    { model = subModel
                    , lift = TransactionMsg
                    , session = getSession ()
                    }
            in
            toPageWithGlobalMsgs (Transaction paymentId) TransactionMsg (Transaction.update ctx) subMsg

        ( TransactionListMsg subMsg, TransactionList subModel ) ->
            let
                ctx =
                    { model = subModel
                    , lift = TransactionListMsg
                    , session = getSession ()
                    }
            in
            toPageWithGlobalMsgs TransactionList TransactionListMsg (TransactionList.update ctx) subMsg

        ( _, NotFound ) ->
            -- Disregard incoming messages when we're on the
            -- NotFound page.
            ( model
            , Cmd.none
            )

        ( _, _ ) ->
            -- Disregard incoming messages that arrived for the wrong page
            ( model
            , Cmd.none
            )



-- HELPERS --


setRoute : Maybe Route -> Model -> ( Model, Cmd Msg )
setRoute maybeRoute model =
    let
        transition toMsg task =
            ( { model | pageState = TransitioningFrom (getPage model.pageState) }
            , Task.attempt toMsg task
            )

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
            ( { model | pageState = Loaded NotFound }
            , Cmd.none
            )

        Just Route.Login ->
            ( { model | pageState = Loaded (Login Login.initialModel) }
            , Cmd.none
            )

        Just Route.Logout ->
            ( { model | session = GuestSession }
            , Route.modifyUrl model Route.Login
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
                    ( { model | pageState = Loaded (NewTransaction <| subModel) }
                    , Cmd.map NewTransactionMsg subMsg
                    )

                GuestSession ->
                    setRoute (Just Route.Login) model

        Just Route.Balances ->
            whenLogged
                (\session ->
                    let
                        ( subModel, subMsg ) =
                            Balances.init session
                    in
                    ( { model | pageState = Loaded (Balances <| subModel) }
                    , Cmd.map BalancesMsg subMsg
                    )
                )

        Just (Route.Transaction paymentId) ->
            whenLogged
                (\session ->
                    let
                        ( subModel, subMsg ) =
                            Transaction.init session paymentId
                    in
                    ( { model | pageState = Loaded (Transaction paymentId <| subModel) }
                    , Cmd.map TransactionMsg subMsg
                    )
                )

        Just Route.TransactionList ->
            whenLogged
                (\session ->
                    let
                        ( subModel, subMsg ) =
                            TransactionList.init session
                    in
                    ( { model | pageState = Loaded (TransactionList <| subModel) }
                    , Cmd.map TransactionListMsg subMsg
                    )
                )


getPage : PageState -> Page
getPage pageState =
    case pageState of
        Loaded page ->
            page

        TransitioningFrom page ->
            page
