namespace Tests

open NUnit.Framework
open Backend.Repo
open System

[<TestClass>]
type TestClass () =

    [<SetUp>]
    member this.Setup () =
        ()

    [<Test>]
    member this.Test1 () =
        let transactions : Transaction list =
          [ (1, [2],     1000);  //1 -> +1000, 2 -> -1000
            (1, [1;2],   2000);  //1 -> +1000, 2 -> -1000
            (1, [1;2;3], 3000);  //1 -> +2000, 2 -> -1000, 3 -> -1000
            (1, [2;3],   2000);  //1 -> +2000, 2 -> -1000, 3 -> -1000
          ]                      //1 =  +6000, 2 =  -4000, 3 =  -2000
          |> List.map (fun (payorId, beneficientIds, amount) ->
            {authorId = payorId; payorId = payorId; beneficientIds = beneficientIds; acceptanceIds = []; amount = amount; updatedAt = DateTime.Now }
          )

        let totalBalance12 = calculateTotalBalanceFromTransactionsFor2Users 1 2 transactions
        let expectedBalance12 : Fraction = {num = 4000; den = 1}
        Assert.That(totalBalance12, Is.EqualTo(expectedBalance12))

        let totalBalance13 = calculateTotalBalanceFromTransactionsFor2Users 1 3 transactions
        let expectedBalance13 : Fraction = {num = 2000; den = 1}
        Assert.That(totalBalance13, Is.EqualTo(expectedBalance13))

        let totalBalance23 = calculateTotalBalanceFromTransactionsFor2Users 2 3 transactions
        let expectedBalance23 : Fraction = {num = 0; den = 1}
        Assert.That(totalBalance23, Is.EqualTo(expectedBalance23))
        
    [<Test>]
    member this.Test2 () =
        let transactions : Transaction list =
          [ (2, [1],     100000);  //2 -> +1000, 1 -> -1000
            (2, [1;2],   200000);  //2 -> +1000, 1 -> -1000
            (2, [1;2;3], 300000);  //2 -> +2000, 1 -> -1000, 3 -> -1000
            (2, [1;3],   200000);  //2 -> +2000, 1 -> -1000, 3 -> -1000
            (4, [4;5],   190900)
          ]                      //2 =  +6000, 1 =  -4000, 3 =  -2000
          |> List.map (fun (payorId, beneficientIds, amount) ->
             { authorId = payorId; payorId = payorId; beneficientIds = beneficientIds;
               acceptanceIds = []; amount = amount; updatedAt = DateTime.Now } )
        
        let calc uid1 uid2 = calculateTotalBalanceFromTransactionsFor2Users uid1 uid2 transactions

        Assert.That(calc 1 2, Is.EqualTo({num = -400000; den = 1}))
        Assert.That(calc 2 1, Is.EqualTo({num = -400000; den = 1}))
 
        Assert.That(calc 2 3, Is.EqualTo({num = 200000; den = 1}))
 
        Assert.That(calc 1 3, Is.EqualTo({num = 0; den = 1}))

        Assert.That(calc 4 5, Is.EqualTo({num = 190900/2; den = 1}))
        Assert.That(calc 5 4, Is.EqualTo({num = 190900/2; den = 1}))

        