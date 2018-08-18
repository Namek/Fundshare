module Data.Context exposing (..)

import Data.Session exposing (..)
import Material
import Route exposing (Route)


type alias ContextData model localMsg rootMsg =
    { model : model
    , mdl : Material.Model
    , lift : localMsg -> rootMsg
    , liftMaterial : Material.Msg rootMsg -> rootMsg
    , matId : List Int -> List Int
    }


type alias Logged ctx =
    { ctx | session : Session }


passContext ctx modelGetter lift =
    { ctx
        | model = modelGetter ctx.model
        , lift = lift
    }


{-| Supposed to use with `Logged ctx`
-}
passContextWithoutSession ctx modelGetter lift subMatId =
    { model = modelGetter ctx.model
    , mdl = ctx.mdl
    , lift = lift
    , liftMaterial = ctx.liftMaterial
    , matId = ctx.matId subMatId |> List.append
    }


type GlobalMsg
    = Navigate Route
    | SetSession (Maybe Session)


matId : List Int -> List Int -> List Int
matId rootId subId =
    List.append rootId subId
