-- Do not manually edit this file, it was auto-generated by dillonkearns/elm-graphql
-- https://github.com/dillonkearns/elm-graphql


module Api.Object.User exposing (InboxTransactionsOptionalArguments, MailboxTransactionsOptionalArguments, OutboxTransactionsOptionalArguments, TransactionsOptionalArguments, balances, email, id, inboxTransactions, mailboxTransactions, name, outboxTransactions, transactions)

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
import Json.Decode as Decode


balances : SelectionSet decodesTo Api.Object.BalanceToOtherUser -> SelectionSet (List decodesTo) Api.Object.User
balances object_ =
    Object.selectionForCompositeField "balances" [] object_ (identity >> Decode.list)


email : SelectionSet String Api.Object.User
email =
    Object.selectionForField "String" "email" [] Decode.string


id : SelectionSet Int Api.Object.User
id =
    Object.selectionForField "Int" "id" [] Decode.int


type alias InboxTransactionsOptionalArguments =
    { offset : OptionalArgument Int
    , limit : OptionalArgument Int
    }


{-| Transactions coming from other users and not yet accepted by this user.
-}
inboxTransactions : (InboxTransactionsOptionalArguments -> InboxTransactionsOptionalArguments) -> SelectionSet decodesTo Api.Object.UserTransaction -> SelectionSet (List decodesTo) Api.Object.User
inboxTransactions fillInOptionals object_ =
    let
        filledInOptionals =
            fillInOptionals { offset = Absent, limit = Absent }

        optionalArgs =
            [ Argument.optional "offset" filledInOptionals.offset Encode.int, Argument.optional "limit" filledInOptionals.limit Encode.int ]
                |> List.filterMap identity
    in
    Object.selectionForCompositeField "inboxTransactions" optionalArgs object_ (identity >> Decode.list)


type alias MailboxTransactionsOptionalArguments =
    { offset : OptionalArgument Int
    , limit : OptionalArgument Int
    }


{-| Both inbox and outbox transactions.
-}
mailboxTransactions : (MailboxTransactionsOptionalArguments -> MailboxTransactionsOptionalArguments) -> SelectionSet decodesTo Api.Object.UserTransaction -> SelectionSet (List decodesTo) Api.Object.User
mailboxTransactions fillInOptionals object_ =
    let
        filledInOptionals =
            fillInOptionals { offset = Absent, limit = Absent }

        optionalArgs =
            [ Argument.optional "offset" filledInOptionals.offset Encode.int, Argument.optional "limit" filledInOptionals.limit Encode.int ]
                |> List.filterMap identity
    in
    Object.selectionForCompositeField "mailboxTransactions" optionalArgs object_ (identity >> Decode.list)


name : SelectionSet String Api.Object.User
name =
    Object.selectionForField "String" "name" [] Decode.string


type alias OutboxTransactionsOptionalArguments =
    { offset : OptionalArgument Int
    , limit : OptionalArgument Int
    }


{-| Transactions made by or accepted by this user or made by but edited by someone else.
-}
outboxTransactions : (OutboxTransactionsOptionalArguments -> OutboxTransactionsOptionalArguments) -> SelectionSet decodesTo Api.Object.UserTransaction -> SelectionSet (List decodesTo) Api.Object.User
outboxTransactions fillInOptionals object_ =
    let
        filledInOptionals =
            fillInOptionals { offset = Absent, limit = Absent }

        optionalArgs =
            [ Argument.optional "offset" filledInOptionals.offset Encode.int, Argument.optional "limit" filledInOptionals.limit Encode.int ]
                |> List.filterMap identity
    in
    Object.selectionForCompositeField "outboxTransactions" optionalArgs object_ (identity >> Decode.list)


type alias TransactionsOptionalArguments =
    { offset : OptionalArgument Int
    , limit : OptionalArgument Int
    }


{-| Transactions for which this user was a payor or a beneficient
-}
transactions : (TransactionsOptionalArguments -> TransactionsOptionalArguments) -> SelectionSet decodesTo Api.Object.UserTransaction -> SelectionSet (List decodesTo) Api.Object.User
transactions fillInOptionals object_ =
    let
        filledInOptionals =
            fillInOptionals { offset = Absent, limit = Absent }

        optionalArgs =
            [ Argument.optional "offset" filledInOptionals.offset Encode.int, Argument.optional "limit" filledInOptionals.limit Encode.int ]
                |> List.filterMap identity
    in
    Object.selectionForCompositeField "transactions" optionalArgs object_ (identity >> Decode.list)
