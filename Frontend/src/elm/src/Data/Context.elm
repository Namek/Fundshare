module Data.Context exposing (ContextData, GlobalMsg(..), Logged, passContext, passContextWithoutSession, subContext)

import Data.CommonData exposing (CommonData)
import Data.Session exposing (..)
import Date exposing (Date)
import Route exposing (Route)


{-| Typical usage - a context type for specific Page:

    type alias Context msg =
        ContextData Model Msg msg

-}
type alias ContextData model localMsg rootMsg =
    { model : model
    , lift : localMsg -> rootMsg
    , todayDate : Date
    }


{-| Typical usage - a context type for specific Page that needs user to be logged in to see the page:

    type alias Context msg =
        Logged (ContextData Model Msg msg)

-}
type alias Logged ctx =
    { ctx
        | session : Session
        , commonData : CommonData
    }


passContext ctx modelGetter lift =
    { ctx
        | model = modelGetter ctx.model
        , lift = lift
    }


{-| Supposed to be used with `Logged ctx`
-}
passContextWithoutSession ctx modelGetter lift =
    { model = modelGetter ctx.model
    , lift = lift
    , todayDate = ctx.todayDate
    }


{-| Useful for subcomponents
-}
subContext :
    Logged (ContextData model localMsg rootMsg)
    -> (model -> subModel)
    -> (subMsg -> localMsg)
    -> Logged (ContextData subModel subMsg rootMsg)
subContext ctx modelGetter subMsg =
    { model = modelGetter ctx.model
    , lift = ctx.lift << subMsg
    , todayDate = ctx.todayDate
    , session = ctx.session
    , commonData = ctx.commonData
    }


type GlobalMsg
    = Navigate Route
    | SetSession (Maybe Session)
      -- TODO: instead changing it directly, request a refresh from backend?
    | UpdateInboxSize Int
      -- it's useful to hide scrollbars in the window for a modal window
    | SetScrollbarsVisibility Bool
