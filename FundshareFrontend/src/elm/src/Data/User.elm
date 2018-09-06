module Data.User exposing (User, UserId(..), decoder, encode, userIdToString)

import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as Pipeline exposing (decode, required)
import Json.Encode as Encode exposing (Value)


type alias User =
    { id : Int
    , email : String
    , name : String
    }


type UserId
    = UserId Int


userIdToString : UserId -> String
userIdToString (UserId userId) =
    toString userId


decoder : Decoder User
decoder =
    decode User
        |> required "id" Decode.int
        |> required "email" Decode.string
        |> required "name" Decode.string


encode : User -> Value
encode user =
    Encode.object
        [ ( "id", Encode.int user.id )
        , ( "email", Encode.string user.email )
        , ( "name", Encode.string user.name )
        ]
