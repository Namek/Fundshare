module Data.Balance exposing (Balance)

import Data.Person exposing (PersonId)
import Time exposing (Posix)


type alias Balance =
    { personId : PersonId
    , name : String
    , value : Float
    , iHaveMore : Bool
    , sharedPaymentCount : Int
    , transferCount : Int
    , unseenForMeCount : Int
    , lastUpdateAt : Maybe Posix
    }
