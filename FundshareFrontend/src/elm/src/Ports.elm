port module Ports exposing (onSessionChange)

import Json.Encode exposing (Value)


-- port storeToken : Maybe String -> Cmd msg


port onSessionChange : (Value -> msg) -> Sub msg
