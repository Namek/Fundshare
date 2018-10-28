module Main exposing (main)

import Browser exposing (Document)
import Browser.Navigation as Nav
import Cmd.Extra
import Data.Context exposing (GlobalMsg(..))
import Data.Session as Session exposing (Session, SessionState(..))
import Data.Transaction exposing (TransactionId)
import Data.User exposing (User, UserId(..))
import Element exposing (Element, paragraph, text)
import Graphql.Http
import Html exposing (Html)
import Json.Decode as Decode exposing (Value)
import Maybe.Extra
import Misc exposing (noCmd)
import Page.Balances as Balances
import Page.Errored as Errored exposing (PageLoadError(..))
import Page.Inbox as Inbox
import Page.Login as Login
import Page.NewTransaction as NewTransaction
import Page.NotFound as NotFound
import Page.Transaction as Transaction
import Page.TransactionHistory as TransactionHistory
import RemoteData exposing (RemoteData)
import Request.Common exposing (sendMutationRequest)
import Request.Session exposing (SignInResult, checkSession)
import Route exposing (Route, modifyUrl)
import Task
import Url exposing (Url)
import Views.Page as Page exposing (Page(..), frame)



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
    , page : Page
    , lastLocation : Url
    , session : SessionState
    }


init : Value -> Url -> Nav.Key -> ( Model, Cmd Msg )
init json url navKey =
    {- check authorization before setting location from URL -}
    let
        model =
            { navKey = navKey
            , page = initialPage
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
        [ Element.layout [] (model.page |> viewPage model)
        ]
    }


viewPage : Model -> Page -> Element Msg
viewPage model page =
    let
        isLoggedIn =
            model.session /= GuestSession

        frame =
            Page.frame HandleGlobalMsg isLoggedIn model.session

        getAuthorizedSession =
            \() ->
                case model.session of
                    LoggedSession session ->
                        session

                    _ ->
                        Debug.todo "you have to be authorized!"

        pageView =
            case page of
                Blank ->
                    paragraph [] [ text "" ]

                NotFound ->
                    NotFound.view model.session

                Errored subModel ->
                    Errored.view model.session subModel

                Login subModel ->
                    Login.view { model = subModel, lift = LoginMsg }

                NewTransaction subModel ->
                    NewTransaction.view
                        { model = subModel
                        , lift = NewTransactionMsg
                        , session = getAuthorizedSession ()
                        }

                Balances subModel ->
                    Balances.view
                        { model = subModel
                        , lift = BalancesMsg
                        , session = getAuthorizedSession ()
                        }

                Transaction paymentId subModel ->
                    Transaction.view
                        { model = subModel
                        , lift = TransactionMsg
                        , session = getAuthorizedSession ()
                        }

                Inbox subModel ->
                    Inbox.view
                        { model = subModel
                        , lift = InboxMsg
                        , session = getAuthorizedSession ()
                        }

                TransactionHistory subModel ->
                    TransactionHistory.view
                        { model = subModel
                        , lift = TransactionHistoryMsg
                        , session = getAuthorizedSession ()
                        }
    in
    frame pageView



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
    | CheckAuthSession
    | CheckAuthSession_Response (RemoteData (Graphql.Http.Error (Maybe SignInResult)) (Maybe SignInResult))
    | LoginMsg Login.Msg
    | NewTransactionMsg NewTransaction.Msg
    | BalancesMsg Balances.Msg
    | TransactionMsg Transaction.Msg
    | InboxMsg Inbox.Msg
    | TransactionHistoryMsg TransactionHistory.Msg


pageErrored : Model -> String -> ( Model, Cmd msg )
pageErrored model errorMessage =
    let
        error =
            Errored.pageLoadError errorMessage
    in
    ( { model | page = Errored error }
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
            case Route.fromUrl url of
                Just route ->
                    initRoute (Just route) { model | lastLocation = url }

                Nothing ->
                    model |> noCmd

        UrlRequested req ->
            case req of
                Browser.Internal url ->
                    case Route.fromUrl url of
                        Just route ->
                            ( model, Route.modifyUrl model route )

                        Nothing ->
                            model |> noCmd

                Browser.External href ->
                    ( model, Nav.load href )

        CheckAuthSession ->
            let
                cmd =
                    checkSession ()
                        |> sendMutationRequest CheckAuthSession_Response
            in
            ( model, cmd )

        {- first thing that comes from this app to backend - decide whether user is still logged in -}
        CheckAuthSession_Response (RemoteData.Success maybeUser) ->
            case maybeUser of
                Just user ->
                    let
                        modelWithSession =
                            { model | session = LoggedSession { user = user } }

                        newRoute =
                            Route.fromUrl model.lastLocation
                                |> Maybe.withDefault Route.Balances

                        rerouteCmd =
                            Route.modifyUrl modelWithSession newRoute
                    in
                    ( modelWithSession, rerouteCmd )

                Nothing ->
                    {- no valid token, guest session -}
                    ( { model | session = GuestSession }
                    , Route.modifyUrl model Route.Login
                    )

        CheckAuthSession_Response (RemoteData.Failure err) ->
            {- we don't know but let's just say it's a guest session -}
            { model | session = GuestSession } |> noCmd

        CheckAuthSession_Response _ ->
            model |> noCmd

        _ ->
            updatePage model.page msg model


updatePage : Page -> Msg -> Model -> ( Model, Cmd Msg )
updatePage page msg model =
    let
        toPageWithGlobalMsgs pageModelHandler msgLifter subUpdate subMsg =
            let
                ( ( pageModel, cmd ), globalMsg ) =
                    subUpdate subMsg

                cmds =
                    Cmd.batch
                        [ Cmd.map msgLifter cmd
                        , Cmd.map HandleGlobalMsg globalMsg
                        ]
            in
            ( { model | page = pageModelHandler pageModel }, cmds )

        buildCtx subModel lift session =
            { model = subModel
            , lift = lift
            , session = session
            }

        buildGuestCtx subModel lift =
            { model = subModel
            , lift = lift
            }
    in
    case ( msg, page, model.session ) of
        ( LoginMsg subMsg, Login subModel, GuestSession ) ->
            let
                ctx =
                    buildGuestCtx subModel LoginMsg
            in
            toPageWithGlobalMsgs Login LoginMsg (Login.update ctx) subMsg

        ( NewTransactionMsg subMsg, NewTransaction subModel, LoggedSession session ) ->
            let
                ctx =
                    buildCtx subModel NewTransactionMsg session
            in
            toPageWithGlobalMsgs NewTransaction NewTransactionMsg (NewTransaction.update ctx) subMsg

        ( BalancesMsg subMsg, Balances subModel, LoggedSession session ) ->
            let
                ctx =
                    buildCtx subModel BalancesMsg session
            in
            toPageWithGlobalMsgs Balances BalancesMsg (Balances.update ctx) subMsg

        ( TransactionMsg subMsg, Transaction paymentId subModel, LoggedSession session ) ->
            let
                ctx =
                    buildCtx subModel TransactionMsg session
            in
            toPageWithGlobalMsgs (Transaction paymentId) TransactionMsg (Transaction.update ctx) subMsg

        ( InboxMsg subMsg, Inbox subModel, LoggedSession session ) ->
            let
                ctx =
                    buildCtx subModel InboxMsg session
            in
            toPageWithGlobalMsgs Inbox InboxMsg (Inbox.update ctx) subMsg

        ( TransactionHistoryMsg subMsg, TransactionHistory subModel, LoggedSession session ) ->
            let
                ctx =
                    buildCtx subModel TransactionHistoryMsg session
            in
            toPageWithGlobalMsgs TransactionHistory TransactionHistoryMsg (TransactionHistory.update ctx) subMsg

        ( _, NotFound, _ ) ->
            -- Disregard incoming messages when we're on the
            -- NotFound page.
            model |> noCmd

        ( _, _, _ ) ->
            -- Disregard incoming messages that arrived for the wrong page
            model |> noCmd



-- HELPERS --


{-| This does not change URL in a browser, it simply sets state in the model.
-}
initRoute : Maybe Route -> Model -> ( Model, Cmd Msg )
initRoute maybeRoute model =
    let
        errored =
            pageErrored model

        whenLogged func =
            case model.session of
                LoggedSession session ->
                    func session

                GuestSession ->
                    initRoute (Just Route.Login) model

        initWhenLogged initFn pageStateHolder msgLift =
            whenLogged
                (\session ->
                    let
                        ( subModel, subMsg ) =
                            initFn session
                    in
                    ( { model | page = pageStateHolder <| subModel }
                    , Cmd.map msgLift subMsg
                    )
                )
    in
    case maybeRoute of
        Nothing ->
            ( { model | page = NotFound }
            , Cmd.none
            )

        Just Route.Login ->
            ( { model | page = Login Login.initialModel }
            , Cmd.none
            )

        Just Route.Logout ->
            ( { model | session = GuestSession }
            , Route.modifyUrl model Route.Login
            )

        Just Route.NewTransaction ->
            initWhenLogged NewTransaction.init Page.NewTransaction NewTransactionMsg

        Just Route.Balances ->
            initWhenLogged Balances.init Page.Balances BalancesMsg

        Just (Route.Transaction paymentId) ->
            initWhenLogged (Transaction.init paymentId) (Page.Transaction paymentId) TransactionMsg

        Just Route.Inbox ->
            initWhenLogged Inbox.init Page.Inbox InboxMsg

        Just (Route.TransactionHistory pageNo) ->
            initWhenLogged (TransactionHistory.init pageNo) Page.TransactionHistory TransactionHistoryMsg
