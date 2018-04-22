namespace Tests

open FSharp_OOP.Computer
open FSharp_OOP.DarkNet
open Microsoft.VisualStudio.TestTools.UnitTesting
open System.Collections.Generic

module Assert = 
    /// <summary>
    /// Проверяет, выдаёт ли функция исключение заданного типа 'a
    /// </summary>
    /// <param name="f"> Функция, которая должна выдать исключение </param>
    let Throws<'a> f =
        let mutable wasThrown = false
        try
            f()
        with
        | ex -> Assert.AreEqual(ex.GetType(), typedefof<'a>, (sprintf "Actual Exception: %A" ex)); wasThrown <- true
        Assert.IsTrue(wasThrown, "No exception thrown")

[<TestClass>]
type GameTestClass () =

    let r = FSharp_OOP.Computer.StandartRandom(100)
    let computers = [
                    new Computer(r, MacOS); 
                    new Computer(r, Windows);
                    new Computer(r, Windows); 
                    new Computer(r, Linux);
                    new Computer(r, Linux);
                    new Computer(r, MacOS);
                    ]

    let connectionMatrix = [
                            [false; false; true; false; false; true]; 
                            [false; false; true; false; false; false]; 
                            [true; true; false; true; true; false];
                            [false; false; true; false; false; false];
                            [false; false; true; false; false; false];
                            [true; false; false; false; false; false];
                           ]

    [<TestMethod>]
    member this.``Network Test1``() = 
        computers.[1].Infect()

        let network = new Network((fun n -> n.Infected.Length = 3), computers, connectionMatrix)

        Assert.IsTrue(network.Uninfected.Length = 5)

    [<TestMethod>]
    member this.``Network Test2``() = 
        computers.[2].Infect()

        let network = new Network((fun n -> n.Infected.Length = 3), computers, connectionMatrix)
        network.Play() |> ignore

        Assert.AreEqual(network.Infected.Length, 3) (* Проверка, что выполнено условие игры *)

    [<TestMethod>]
    member this.``Network Test3``() = 
        (* Выдаёт исключение, ибо ни один из компьютеров не заражён *)
        Assert.Throws<KeyNotFoundException>(fun () -> Network((fun n -> n.Infected.Length = 3), computers, connectionMatrix) |> ignore)
