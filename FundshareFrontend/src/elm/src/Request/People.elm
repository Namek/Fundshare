module Request.People exposing (getPeople)

import Api.Object.User as User
import Api.Query as Query
import Api.Scalar
import Data.Person exposing (Person, PersonId)
import Graphql.Field as Field
import Graphql.Operation exposing (RootQuery)
import Graphql.SelectionSet exposing (SelectionSet, with)


getPeople : SelectionSet (List Person) RootQuery
getPeople =
    Query.selection identity
        |> with
            (Query.users
                (User.selection Person
                    |> with User.id
                    |> with User.name
                )
            )
