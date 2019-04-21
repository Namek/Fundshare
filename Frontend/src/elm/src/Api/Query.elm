-- Do not manually edit this file, it was auto-generated by dillonkearns/elm-graphql
-- https://github.com/dillonkearns/elm-graphql


module Api.Query exposing (UserRequiredArguments, currentUser, user, users)

import Api.InputObject
import Api.Interface
import Api.Object
import Api.Scalar
import Api.ScalarCodecs
import Api.Union
import Graphql.Internal.Builder.Argument as Argument exposing (Argument)
import Graphql.Internal.Builder.Object as Object
import Graphql.Internal.Encode as Encode exposing (Value)
import Graphql.Operation exposing (RootMutation, RootQuery, RootSubscription)
import Graphql.OptionalArgument exposing (OptionalArgument(..))
import Graphql.SelectionSet exposing (SelectionSet)
import Json.Decode as Decode exposing (Decoder)


{-| Get currently logged in user
-}
currentUser : SelectionSet decodesTo Api.Object.User -> SelectionSet (Maybe decodesTo) RootQuery
currentUser object_ =
    Object.selectionForCompositeField "currentUser" [] object_ (identity >> Decode.nullable)


type alias UserRequiredArguments =
    { id : Int }


{-| Specified user
-}
user : UserRequiredArguments -> SelectionSet decodesTo Api.Object.User -> SelectionSet (Maybe decodesTo) RootQuery
user requiredArgs object_ =
    Object.selectionForCompositeField "user" [ Argument.required "id" requiredArgs.id Encode.int ] object_ (identity >> Decode.nullable)


{-| All users
-}
users : SelectionSet decodesTo Api.Object.User -> SelectionSet (List decodesTo) RootQuery
users object_ =
    Object.selectionForCompositeField "users" [] object_ (identity >> Decode.list)
