module Data.Person exposing (..)


type alias Person =
    { id : PersonId
    , name : String
    }


type alias PersonId =
    Int
