module Request.People exposing (..)

import Data.Person exposing (Person, PersonId)
import GraphQL.Request.Builder exposing (..)
import GraphQL.Request.Builder.Arg as Arg
import GraphQL.Request.Builder.Variable as Var


getPeople : Request Query (List Person)
getPeople =
    let
        person =
            object Person
                |> with (field "id" [] int)
                |> with (field "name" [] string)
    in
    extract
        (field "users"
            []
            (list person)
        )
        |> namedQueryDocument "users"
        |> request {}
