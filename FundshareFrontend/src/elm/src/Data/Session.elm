module Data.Session exposing
    ( Session
    , SessionState(..)
    )

import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as Pipeline exposing (required)
import Json.Encode as Encode exposing (Value)


{-| If one has access to this data then he's logged in.
-}
type alias Session =
    { id : Int
    , email : String
    , name : String
    , inboxSize : Int
    }


type SessionState
    = LoggedSession Session
    | GuestSession
