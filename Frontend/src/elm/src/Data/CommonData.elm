module Data.CommonData exposing (CommonData)

import Data.Person exposing (Person)
import Date exposing (Date)


{-| Data common for all pages, available to user who is logged in
-}
type alias CommonData =
    { people : List Person
    }
