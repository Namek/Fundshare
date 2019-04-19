module Page.Transaction exposing (Model, Msg, init, update, view)

import Cmd.Extra
import Data.Context exposing (ContextData, GlobalMsg, Logged)
import Data.Session exposing (Session)
import Data.Transaction exposing (TransactionId)
import Element exposing (Element, paragraph, text)
import Html.Events
import Json.Decode as Json
import Request.Common exposing (..)



-- MODEL --


type alias Model =
    { paymentId : TransactionId }


init : TransactionId -> Session -> ( Model, Cmd Msg )
init paymentId session =
    ( { paymentId = paymentId }, Cmd.none )


type alias Context msg =
    Logged (ContextData Model Msg msg)



-- VIEW --


view : Context msg -> Element msg
view { model } =
    paragraph [] [ text <| "payment " ++ String.fromInt model.paymentId ]



-- UPDATE --


type Msg
    = Well


update : Context msg -> Msg -> ( ( Model, Cmd Msg ), Cmd GlobalMsg )
update { model } msg =
    ( ( model, Cmd.none ), Cmd.none )
