module Data.Transaction exposing
    ( Transaction
    , TransactionId
    , amountDifferenceForMyAccount
    , amountToMoney
    , amountToMoneyChange
    , amountToMoneyChangeLeftPad
    , isTransactionInInboxForUser
    )

import Data.Person exposing (PersonId)
import Date exposing (Date)
import List.Extra
import Misc exposing (digitCount, either)
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


isTransactionInInboxForUser personId transaction =
    not <| List.member personId transaction.acceptanceIds


amountToMoney : Int -> Float
amountToMoney amount =
    (amount |> toFloat) / 100


amountToMoneyChange : Bool -> Int -> String
amountToMoneyChange includeSign amount =
    let
        val =
            amountToMoney amount
    in
    if includeSign then
        if val > 0 then
            "+" ++ String.fromFloat val

        else
            String.fromFloat val

    else
        String.fromFloat (abs val)


amountToMoneyChangeLeftPad : Bool -> Int -> Int -> String
amountToMoneyChangeLeftPad includeSign totalWidthOfIntegralPartString amount =
    let
        val =
            abs amount

        integralPart =
            val // 100

        fractionalPart =
            val |> modBy 100

        charDiff =
            includeSign |> either 1 0

        integralPartStr =
            amountToMoneyChange False (integralPart * 100)
                |> String.padLeft
                    (totalWidthOfIntegralPartString - charDiff)
                    ' '

        fractionalPartStr =
            if fractionalPart >= 10 then
                "." ++ String.fromInt fractionalPart

            else if fractionalPart > 0 then
                "." ++ String.fromInt fractionalPart ++ "0"

            else
                ""

        str =
            integralPartStr ++ fractionalPartStr
    in
    if includeSign then
        (amount < 0 |> either "-" "+") ++ str

    else
        str


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
