module Page.Transaction exposing (Model, Msg, init, update, view)

import Cmd.Extra
import Data.Context exposing (ContextData, GlobalMsg, Logged)
import Data.Session exposing (Session)
import Data.Transaction exposing (TransactionId)
import Html exposing (Html, div, p, text)
import Html.Events
import Json.Decode as Json
import Material.Button as Button
import Material.Elevation as Elevation
import Material.Options as Options exposing (css, when)
import Material.Progress as Loading
import Material.Textfield as Textfield
import Material.Typography as Typo
import Misc exposing ((=>))
import Request.Common exposing (..)



-- MODEL --


type alias Model =
    { paymentId : TransactionId }


init : Session -> TransactionId -> ( Model, Cmd Msg )
init session paymentId =
    { paymentId = paymentId }
        => Cmd.none


type alias Context msg =
    Logged (ContextData Model Msg msg)



-- VIEW --


view : Context msg -> Html msg
view { model } =
    div [] [ text <| "payment " ++ toString model.paymentId ]



-- UPDATE --


type Msg
    = Well


update : Context msg -> Msg -> ( ( Model, Cmd Msg ), Cmd GlobalMsg )
update { model } msg =
    model
        => Cmd.none
        => Cmd.none
