module Data.Person exposing (Person, PersonId, personIdToName)

import List.Extra


type alias Person =
    { id : PersonId
    , name : String
    }


type alias PersonId =
    Int


personIdToName : List Person -> Int -> String
personIdToName people pid =
    List.Extra.find (\p -> p.id == pid) people
        |> Maybe.andThen (Just << .name)
        |> Maybe.withDefault ""
