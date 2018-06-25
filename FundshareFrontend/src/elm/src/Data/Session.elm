module Data.Session exposing (..)

import Data.User as User exposing (User)
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as Pipeline exposing (decode, required)
import Json.Encode as Encode exposing (Value)


{-| If one has access to this data then he's logged in.
-}
type alias Session =
    { authToken : String
    , user : User
    }



-- type SessionState
--     = LoggedSession Session
--     | GuestSession


type SessionState
    = LoggedSession Session
    | GuestSession


decoder : Decoder Session
decoder =
    decode Session
        |> required "authToken" Decode.string
        |> required "user" User.decoder


encode : Session -> Value
encode session =
    Encode.object
        [ ( "authToken", Encode.string session.authToken )
        , ( "user", User.encode session.user )
        ]


decodeFromJson : Decode.Value -> SessionState
decodeFromJson json =
    json
        |> Decode.decodeValue Decode.string
        |> Result.toMaybe
        |> Maybe.andThen (Decode.decodeString decoder >> Result.toMaybe)
        |> Maybe.andThen (Just << LoggedSession)
        |> Maybe.withDefault GuestSession
