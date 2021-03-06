module Page.Boilerplate exposing (Model, Msg, init, update, view)

import Cmd.Extra
import Data.Context exposing (ContextData, GlobalMsg, Logged)
import Data.Session exposing (Session)
import Html exposing (Html, div, p, text)
import Html.Events
import Json.Decode as Json
import Material.Button as Button
import Material.Elevation as Elevation
import Material.Options as Options exposing (css, when)
import Material.Progress as Loading
import Material.Textfield as Textfield
import Material.Typography as Typo
import Request.Common exposing (..)



-- MODEL --


type alias Model =
    {}


init : Session -> ( Model, Cmd Msg )
init session =
    ( {}, Cmd.none )


type alias Context msg =
    Logged (ContextData Model Msg msg)



-- VIEW --


view : Context msg -> Element msg
view { model } =
    row [] [ text "balances" ]



-- UPDATE --


type Msg
    = Well


update : Context msg -> Msg -> ( ( Model, Cmd Msg ), Cmd GlobalMsg )
update { model } msg =
    ( ( model, Cmd.none ), Cmd.none )
