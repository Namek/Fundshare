module Request.Session exposing (..)

import Data.Session as Session exposing (Session)
import GraphQL.Request.Builder exposing (..)
import GraphQL.Request.Builder.Arg as Arg
import GraphQL.Request.Builder.Variable as Var
import Json.Encode as Encode
import Ports


type alias SignInResult =
    { token : String, id : Int, name : String }


signIn : { a | email : String, password : String } -> Request Mutation SignInResult
signIn credentials =
    let
        emailVar =
            Var.required "email" .email Var.string

        passwordVar =
            Var.required "password" .password Var.string
    in
    extract
        (field "signIn"
            [ ( "email", Arg.variable emailVar )
            , ( "password", Arg.variable passwordVar )
            ]
            (object SignInResult
                |> with (field "token" [] string)
                |> with (field "id" [] int)
                |> with (field "name" [] string)
            )
        )
        |> mutationDocument
        |> request credentials


storeSession : Session -> Cmd msg
storeSession session =
    Session.encode session
        |> Encode.encode 0
        |> Just
        |> Ports.storeSession
