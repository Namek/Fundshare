module Data.Session exposing (Session, SessionState(..), decodeFromJson, decoder, encode)

import Data.User as User exposing (User)
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as Pipeline exposing (required)
import Json.Encode as Encode exposing (Value)


{-| If one has access to this data then he's logged in.
-}
type alias Session =
    { user : User
    }


type SessionState
    = LoggedSession Session
    | GuestSession


decoder : Decoder Session
decoder =
    Decode.succeed Session
        |> required "user" User.decoder


encode : Session -> Value
encode session =
    Encode.object
        [ ( "user", User.encode session.user )
        ]


decodeFromJson : Decode.Value -> SessionState
decodeFromJson json =
    json
        |> Decode.decodeValue Decode.string
        |> Result.toMaybe
        |> Maybe.andThen (Decode.decodeString decoder >> Result.toMaybe)
        |> Maybe.andThen (Just << LoggedSession)
        |> Maybe.withDefault GuestSession
