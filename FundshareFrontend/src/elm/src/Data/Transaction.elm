module Data.Transaction exposing (Transaction, TransactionId)

import Data.Person exposing (PersonId)
import Time exposing (Posix)


type alias Transaction =
    { id : TransactionId
    , amount : Float
    , description : Maybe String
    , tags : List String
    , payorId : PersonId
    , payeeIds : List PersonId
    , paidAt : Posix
    }


type alias TransactionId =
    Int
