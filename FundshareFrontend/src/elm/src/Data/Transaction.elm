module Data.Transaction exposing (Transaction, TransactionId, amountDifferenceForMyAccount, amountToMoney, isTransactionUnseenForUser)

import Data.Person exposing (PersonId)
import List.Extra
import Time exposing (Posix)


type alias Transaction =
    { id : TransactionId
    , amount : Int
    , description : Maybe String
    , tags : List String
    , payorId : PersonId
    , beneficientIds : List PersonId
    , acceptanceIds : List PersonId
    , insertedAt : Posix
    }


type alias TransactionId =
    Int


isTransactionUnseenForUser personId transaction =
    not <| List.member personId transaction.acceptanceIds


amountToMoney : Int -> Float
amountToMoney amount =
    (amount |> toFloat) / 100


amountDifferenceForMyAccount : PersonId -> Transaction -> Int
amountDifferenceForMyAccount userId transaction =
    let
        totalPeopleCount =
            List.length transaction.beneficientIds

        onePersonPart =
            if totalPeopleCount == 1 then
                transaction.amount

            else
                round ((1 / toFloat totalPeopleCount) * toFloat transaction.amount)
    in
    if transaction.payorId == userId then
        let
            amIBeneficient =
                List.member userId transaction.beneficientIds
        in
        if amIBeneficient then
            transaction.amount - onePersonPart

        else
            transaction.amount

    else
        onePersonPart * -1
