module Data.Balance exposing (Balance)

import Data.Person exposing (PersonId)
import Date exposing (Date)


type alias Balance =
    { personId : PersonId
    , name : String
    , value : Float
    , iHaveMore : Bool
    , sharedPaymentCount : Int
    , transferCount : Int
    , unseenUpdateCount : Int
    , lastUpdateAt : Maybe Date
    }
