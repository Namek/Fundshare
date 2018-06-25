module Data.Transaction exposing (..)

import Data.Person exposing (PersonId)
import Date exposing (Date)


type alias Transaction =
    { id : TransactionId
    , amount : Float
    , description : Maybe String
    , tags : List String
    , payorId : PersonId
    , payeeIds : List PersonId
    , paidAt : Date
    }


type alias TransactionId =
    Int
