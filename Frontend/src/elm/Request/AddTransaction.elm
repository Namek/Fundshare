module Request.AddTransaction exposing (NewTransaction, addTransaction)

import Api.Mutation as Mutation
import Api.Object.UserTransaction as UserTransaction
import Api.Scalar
import Data.Person exposing (PersonId)
import Data.Transaction exposing (TransactionId)
import Graphql.Operation exposing (RootMutation, RootQuery)
import Graphql.OptionalArgument as OptionalArgument exposing (OptionalArgument(..))
import Graphql.SelectionSet as SelectionSet exposing (SelectionSet, with)


type alias NewTransaction =
    { amount : Int
    , payorId : PersonId
    , beneficientIds : List PersonId
    , description : Maybe String
    , tags : Maybe (List String)
    }


addTransaction : NewTransaction -> SelectionSet TransactionId RootMutation
addTransaction newTransaction =
    let
        optionalArgs =
            \optionals ->
                { optionals
                    | description = OptionalArgument.fromMaybe newTransaction.description
                }

        requiredArgs =
            { payorId = newTransaction.payorId
            , beneficientIds = newTransaction.beneficientIds
            , amount = newTransaction.amount
            , tags = newTransaction.tags |> Maybe.withDefault []
            }
    in
    Mutation.addTransaction
        optionalArgs
        requiredArgs
        UserTransaction.id
