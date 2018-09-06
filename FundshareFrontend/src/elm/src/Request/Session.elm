module Request.Session exposing (SignInResult, checkSession, signIn)

import Data.User exposing (User)
import GraphQL.Request.Builder exposing (..)
import GraphQL.Request.Builder.Arg as Arg
import GraphQL.Request.Builder.Variable as Var
import MD5


type alias SignInResult =
    User


signIn : { a | email : String, password : String } -> Request Mutation SignInResult
signIn credentials =
    let
        emailVar =
            Var.required "email" .email Var.string

        passwordVar =
            Var.required "passwordHash" (.password >> MD5.hex) Var.string
    in
    extract
        (field "signIn"
            [ ( "email", Arg.variable emailVar )
            , ( "passwordHash", Arg.variable passwordVar )
            ]
            (object User
                |> with (field "id" [] int)
                |> with (field "email" [] string)
                |> with (field "name" [] string)
            )
        )
        |> namedMutationDocument "SignIn"
        |> request credentials


checkSession : Request Mutation (Maybe SignInResult)
checkSession =
    extract
        (field "checkSession"
            []
            (nullable <|
                (object User
                    |> with (field "id" [] int)
                    |> with (field "email" [] string)
                    |> with (field "name" [] string)
                )
            )
        )
        |> namedMutationDocument "CheckSession"
        |> request ()
