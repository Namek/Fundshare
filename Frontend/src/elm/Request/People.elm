module Request.People exposing (getPeople)

import Api.Object.User as User
import Api.Query as Query
import Data.Person exposing (Person, PersonId)
import Graphql.Operation exposing (RootQuery)
import Graphql.SelectionSet as SelectionSet exposing (SelectionSet, with)


getPeople : SelectionSet (List Person) RootQuery
getPeople =
    Query.users
        (SelectionSet.succeed Person
            |> with User.id
            |> with User.name
        )
