module Main exposing (main)

import Browser exposing (Document)
import Browser.Navigation as Nav
import Cmd.Extra
import Data.CommonData exposing (CommonData)
import Data.Context exposing (GlobalMsg(..))
import Data.Person exposing (Person)
import Data.Session as Session exposing (Session, SessionState(..))
import Date exposing (Date)
import Element exposing (Element, paragraph, text)
import Graphql.Http
import Misc exposing (css, noCmd)
import Page.Balances as Balances
import Page.Errored as Errored exposing (PageLoadError(..))
import Page.Login as Login
import Page.Mailbox as Mailbox
import Page.NewTransaction as NewTransaction
import Page.NotFound as NotFound
import Page.Transaction as Transaction
import Page.TransactionHistory as TransactionHistory
import RemoteData exposing (RemoteData)
import Request.Common exposing (sendMutationRequest, sendQueryRequest)
import Request.People exposing (getPeople)
import Request.Session exposing (SignInResult, SignOutResult, checkSession, signOut)
import Route exposing (Route, modifyUrl)
import Url exposing (Url)
import Views.Page as Page exposing (ActivePage(..), Page(..), frame)



-- MAIN --


main : Program { day : Int, month : Int, year : Int } Model Msg
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
    , hideScrollbars : Bool
    , session : SessionState
    , todayDate : Date
    , commonData : CommonData
    }


init : { day : Int, month : Int, year : Int } -> Url -> Nav.Key -> ( Model, Cmd Msg )
init { day, month, year } url navKey =
    {- check authorization before setting location from URL -}
    let
        model =
            { navKey = navKey
            , page = initialPage
            , lastLocation = url
            , hideScrollbars = False
            , session = GuestSession
            , todayDate = Date.fromCalendarDate year (Date.numberToMonth month) day
            , commonData =
                { people = []
                }
            }
    in
    ( model, Cmd.Extra.perform CheckAuthSession )


initialPage : Page
initialPage =
    Blank



-- VIEW --


view : Model -> Document Msg
view model =
    let
        bodyAttrs =
            if model.hideScrollbars then
                [ css "overflow" "hidden", css "height" "100%", css "width" "100%" ]

            else
                []
    in
    { title = "Fundshare"
    , body =
        [ Element.layout bodyAttrs (model.page |> viewPage model)
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

        getAuthorizedCommonData =
            \() ->
                case model.session of
                    LoggedSession _ ->
                        model.commonData

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
                    Login.view
                        { model = subModel
                        , lift = LoginMsg
                        , todayDate = model.todayDate
                        }

                NewTransaction subModel ->
                    NewTransaction.view
                        { model = subModel
                        , lift = NewTransactionMsg
                        , todayDate = model.todayDate
                        , session = getAuthorizedSession ()
                        , commonData = getAuthorizedCommonData ()
                        }

                Balances subModel ->
                    Balances.view
                        { model = subModel
                        , lift = BalancesMsg
                        , todayDate = model.todayDate
                        , session = getAuthorizedSession ()
                        , commonData = getAuthorizedCommonData ()
                        }

                Transaction paymentId subModel ->
                    Transaction.view
                        { model = subModel
                        , lift = TransactionMsg
                        , todayDate = model.todayDate
                        , session = getAuthorizedSession ()
                        , commonData = getAuthorizedCommonData ()
                        }

                Mailbox subModel ->
                    Mailbox.view
                        { model = subModel
                        , lift = MailboxMsg
                        , todayDate = model.todayDate
                        , session = getAuthorizedSession ()
                        , commonData = getAuthorizedCommonData ()
                        }

                TransactionHistory subModel ->
                    TransactionHistory.view
                        { model = subModel
                        , lift = TransactionHistoryMsg
                        , todayDate = model.todayDate
                        , session = getAuthorizedSession ()
                        , commonData = getAuthorizedCommonData ()
                        }

        activePage =
            case page of
                NewTransaction _ ->
                    Route_NewTransaction

                Balances _ ->
                    Route_Balances

                Mailbox _ ->
                    Route_Mailbox

                TransactionHistory _ ->
                    Route_History

                _ ->
                    Route_Unknown
    in
    frame activePage pageView



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
    | GetCommonData_Response (RemoteData (Graphql.Http.Error (List Person)) (List Person))
    | SignOut_Response (RemoteData (Graphql.Http.Error SignOutResult) SignOutResult)
    | LoginMsg Login.Msg
    | NewTransactionMsg NewTransaction.Msg
    | BalancesMsg Balances.Msg
    | TransactionMsg Transaction.Msg
    | MailboxMsg Mailbox.Msg
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
    let
        queryCommonData =
            \() -> getPeople |> sendQueryRequest GetCommonData_Response
    in
    case msg of
        HandleGlobalMsg globalMsg ->
            case globalMsg of
                Navigate route ->
                    ( model, Route.modifyUrl model route )

                SetSession (Just session) ->
                    ( { model | session = LoggedSession session }, queryCommonData () )

                SetSession Nothing ->
                    -- TODO: logout
                    ( { model | session = GuestSession }, Cmd.none )

                UpdateInboxSize inboxSize ->
                    let
                        updatedSession =
                            case model.session of
                                LoggedSession session ->
                                    LoggedSession { session | inboxSize = inboxSize }

                                whatever ->
                                    whatever
                    in
                    { model | session = updatedSession } |> noCmd

                SetScrollbarsVisibility visible ->
                    { model | hideScrollbars = not visible } |> noCmd

        -- it's called when user first enters URL of website or back/forward is clicked
        UrlChanged url ->
            case Route.fromUrl url of
                Just route ->
                    initRoute (Just route) { model | lastLocation = url }

                _ ->
                    model |> noCmd

        UrlRequested req ->
            case req of
                Browser.Internal url ->
                    case Route.fromUrl url of
                        Just route ->
                            let
                                routeCmd =
                                    Route.modifyUrl model route
                            in
                            ( model, routeCmd )

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

        {- Auth session is the first thing that comes from this app to backend - decides whether user is still logged in.
           Then, we request for some data common for all pages. If we'll get it, reroute to the first page.
        -}
        CheckAuthSession_Response (RemoteData.Success maybeAuth) ->
            case maybeAuth of
                Just session ->
                    let
                        modelWithSession =
                            { model | session = LoggedSession session }
                    in
                    ( modelWithSession, queryCommonData () )

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

        GetCommonData_Response result ->
            let
                maybeNewModel =
                    result
                        |> RemoteData.andThen
                            (\people ->
                                let
                                    commonData =
                                        model.commonData
                                in
                                RemoteData.Success <| { model | commonData = { commonData | people = people } }
                            )

                newModel =
                    maybeNewModel |> RemoteData.withDefault model

                cmd =
                    if RemoteData.isSuccess maybeNewModel then
                        let
                            newRoute =
                                Route.fromUrl model.lastLocation
                                    |> Maybe.withDefault Route.Balances

                            rerouteCmd =
                                Route.modifyUrl newModel newRoute
                        in
                        rerouteCmd

                    else
                        Cmd.none
            in
            ( newModel, cmd )

        SignOut_Response _ ->
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
            , todayDate = model.todayDate
            , session = session
            , commonData = model.commonData
            }

        buildGuestCtx subModel lift =
            { model = subModel
            , lift = lift
            , todayDate = model.todayDate
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

        ( MailboxMsg subMsg, Mailbox subModel, LoggedSession session ) ->
            let
                ctx =
                    buildCtx subModel MailboxMsg session
            in
            toPageWithGlobalMsgs Mailbox MailboxMsg (Mailbox.update ctx) subMsg

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
            , Cmd.batch
                [ signOut () |> sendMutationRequest SignOut_Response
                , Route.modifyUrl model Route.Login
                ]
            )

        Just Route.NewTransaction ->
            initWhenLogged NewTransaction.init Page.NewTransaction NewTransactionMsg

        Just Route.Balances ->
            initWhenLogged Balances.init Page.Balances BalancesMsg

        Just (Route.Transaction paymentId) ->
            initWhenLogged (Transaction.init paymentId) (Page.Transaction paymentId) TransactionMsg

        Just Route.Mailbox ->
            let
                initFn =
                    case model.page of
                        Page.Mailbox subModel ->
                            Mailbox.reinit subModel

                        _ ->
                            Mailbox.init
            in
            initWhenLogged initFn Page.Mailbox MailboxMsg

        Just (Route.TransactionHistory pageNo) ->
            let
                initFn =
                    case model.page of
                        Page.TransactionHistory subModel ->
                            TransactionHistory.reinit subModel pageNo

                        _ ->
                            TransactionHistory.init pageNo
            in
            initWhenLogged initFn Page.TransactionHistory TransactionHistoryMsg
