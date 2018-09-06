module Data.Person exposing (Person, PersonId)


type alias Person =
    { id : PersonId
    , name : String
    }


type alias PersonId =
    Int
