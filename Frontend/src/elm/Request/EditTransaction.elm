module Request.EditTransaction exposing (editTransaction)

import Api.Mutation as Mutation
import Data.Transaction exposing (Transaction, TransactionEdit, TransactionId)
import Graphql.Operation exposing (RootMutation)
import Graphql.OptionalArgument as OptionalArgument exposing (OptionalArgument(..))
import Graphql.SelectionSet as SelectionSet exposing (SelectionSet, with)
import Request.Transactions exposing (transaction)


editTransaction : TransactionId -> TransactionEdit -> SelectionSet Transaction RootMutation
editTransaction transactionId edit =
    let
        optionalArgs =
            \optionals ->
                { optionals
                    | payorId = Present edit.payorId
                    , beneficientIds = Present edit.beneficientIds
                    , amount = Present edit.amount
                    , tags = Present edit.tags
                    , description = OptionalArgument.fromMaybe edit.description
                }

        requiredArgs =
            { id = transactionId }
    in
    Mutation.editTransaction optionalArgs requiredArgs transaction
