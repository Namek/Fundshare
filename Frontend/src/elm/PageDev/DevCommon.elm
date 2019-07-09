module PageDev.DevCommon exposing (Model, Msg(..), init, makeCtx, update, view)

import Data.CommonData exposing (CommonData)
import Data.Context exposing (GlobalMsg)
import Data.Session exposing (Session)
import Date exposing (Date)
import Element exposing (Element, centerX, column, fill, padding, width)
import Html exposing (Html)
import Html.Attributes exposing (property)
import Json.Encode
import Time exposing (Month(..))


type alias Model subModel =
    { subModel : subModel
    , session : Session
    , commonData : CommonData
    }


type Msg msg
    = GotPageMsg msg
    | GotGlobalCmd GlobalMsg


type alias Context subModel msg =
    { model : subModel
    , lift : msg -> Msg msg
    , todayDate : Date
    , session : Session
    , commonData : CommonData
    }


makeCtx : Model subModel -> Context subModel msg
makeCtx model =
    { model = model.subModel
    , lift = GotPageMsg
    , todayDate = Date.fromCalendarDate 2019 Jun 30
    , session = model.session
    , commonData = model.commonData
    }


init : (Session -> ( subModel, Cmd msg )) -> ( Model subModel, Cmd (Msg msg) )
init subInit =
    let
        session : Session
        session =
            { id = 1, email = "some3mail@test.com", name = "User", inboxSize = 10 }

        ( subModel, subCmds ) =
            subInit session

        caughtCmds =
            Cmd.map (Debug.log "init" << GotPageMsg) <| subCmds

        commonData : CommonData
        commonData =
            { people = [ { id = 1, name = "John" }, { id = 2, name = "Christine" } ] }

        model : Model subModel
        model =
            { subModel = subModel
            , session = session
            , commonData = commonData
            }
    in
    ( model, caughtCmds )


update subUpdate handlePageMsg handleGlobalCmdMsg msg model =
    case msg of
        GotPageMsg pageMsg ->
            let
                dbg =
                    Debug.log "GotPageMsg" pageMsg
            in
            case handlePageMsg model pageMsg of
                Nothing ->
                    let
                        -- ctx : Context
                        ctx =
                            makeCtx model

                        ( ( newSubModel, localCmds ), globalCmds ) =
                            subUpdate ctx pageMsg
                    in
                    ( { model | subModel = newSubModel }
                    , Cmd.batch
                        [ Cmd.map (Debug.log "GotPageMsg: update" << GotPageMsg) <| localCmds
                        , Cmd.map (Debug.log "GotPageMsg: update" << GotGlobalCmd) <| globalCmds
                        ]
                    )

                Just ( model2, givenCmds ) ->
                    ( model2, givenCmds )

        GotGlobalCmd globalCmdMsg ->
            let
                dbg =
                    Debug.log "GotGlobalCmd: Msg" globalCmdMsg
            in
            case handleGlobalCmdMsg model globalCmdMsg of
                Just ( newModel, cmds ) ->
                    ( newModel, cmds )

                Nothing ->
                    ( model, Cmd.none )


view : Element msg -> Html msg
view content =
    let
        str =
            Json.Encode.string

        linkStylesheet href =
            Html.node "link" [ property "rel" (str "stylesheet"), property "href" (str href) ] []
    in
    Element.layout [] <|
        column [ width fill, padding 10 ]
            [ Element.html <| linkStylesheet "/css/app.css"
            , Element.html <| linkStylesheet "/css/fontello-embedded.css"
            , Element.el [ centerX ] <| content
            ]
