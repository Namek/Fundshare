module Request.Session exposing (SignInResult, checkSession, signIn)

import Api.Mutation as Mutation
import Api.Object.CheckSessionResult as CheckSessionResult
import Api.Object.SignInResult as SignInResult
import Data.User exposing (User)
import Graphql.Operation exposing (RootMutation, RootQuery)
import Graphql.SelectionSet as SelectionSet exposing (SelectionSet, with)
import MD5
import RemoteData exposing (RemoteData)


type alias SignInResult =
    User


type alias SignInInput a =
    { a | email : String, password : String }


signIn : SignInInput a -> SelectionSet SignInResult RootMutation
signIn credentials =
    let
        input =
            { email = credentials.email
            , passwordHash = credentials.password |> MD5.hex
            }
    in
    Mutation.selection identity
        |> with
            (Mutation.signIn input
                (SignInResult.selection User
                    |> with SignInResult.id
                    |> with SignInResult.email
                    |> with SignInResult.name
                )
            )


checkSession : () -> SelectionSet (Maybe SignInResult) RootMutation
checkSession () =
    Mutation.selection identity
        |> with
            (Mutation.checkSession
                (CheckSessionResult.selection User
                    |> with CheckSessionResult.id
                    |> with CheckSessionResult.email
                    |> with CheckSessionResult.name
                )
            )
