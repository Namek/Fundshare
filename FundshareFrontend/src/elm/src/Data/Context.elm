module Data.Context exposing (ContextData, GlobalMsg(..), Logged, passContext, passContextWithoutSession)

import Data.Session exposing (..)
import Route exposing (Route)


type alias ContextData model localMsg rootMsg =
    { model : model
    , lift : localMsg -> rootMsg
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
passContextWithoutSession ctx modelGetter lift =
    { model = modelGetter ctx.model
    , lift = lift
    }


type GlobalMsg
    = Navigate Route
    | SetSession (Maybe Session)
