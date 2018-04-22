namespace Tests

open FSharp_OOP.BST
open FSharp_OOP.Computer
open FSharp_OOP.DarkNet
open System
open Microsoft.VisualStudio.TestTools.UnitTesting

[<TestClass>]
type TreeTestClass () =

    [<TestMethod>]
    member this.TreeUsingScenario () =
        let tree = Tree<int>()

        tree.Add(2).Add(1).Add(7).Add(8).Add(5).Add(6).Add(3).Add(4) |> ignore
        Assert.AreEqual(tree.Size, 8)

        let mutable list = []
        for value in tree do 
            list <- value :: list
        Assert.AreEqual(list, [8; 7; 6; 5; 4; 3; 2; 1])

        tree.Remove(2).Remove(6) |> ignore
        Assert.AreEqual(tree.Size, 6)

        list <- []
        for value in tree do 
            list <- value :: list
        Assert.AreEqual(list, [8; 7; 5; 4; 3; 1])

        Assert.AreEqual(tree.Contains(8), true)
        Assert.AreEqual(tree.Contains(6), false)
        Assert.AreEqual(tree.IsEmpty, false)

        tree.Clear()
        Assert.AreEqual(tree.Size, 0)
        Assert.AreEqual(tree.IsEmpty, true)