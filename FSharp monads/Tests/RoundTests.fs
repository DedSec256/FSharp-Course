namespace FSharp_monads

open System
open Monads
open Microsoft.VisualStudio.TestTools.UnitTesting

[<TestClass>]
type RoundTestClass () =

    [<TestMethod>]
    member this.SimpleDivide () =
        let actual = 
            round 3 {
                let! a = 2.0 / 12.0
                let! b = 3.5
                return a / b
            }
        let expected =  Some(0.048)
        Assert.AreEqual(expected, actual)

    [<TestMethod>]
    member this.SimpleCalculations () =
        let actual = 
            round 4 {
                let! a = 2.000 + 3.111111
                let! b = 2.0 * a
                return b
            }
        let expected = Some(10.2222)
        Assert.AreEqual(expected, actual)

    [<TestMethod>]
    member this.DivideByZero () =
        let actual = 
            round 3 {
                let! a = 2.5 / 0.0
                return a
            }
        let expected = Some(infinity)
        Assert.AreEqual(expected, actual)

    [<TestMethod>]
    member this.WrongAccuracy () =
        let actual = 
            round -3 {
                let! a = 2.5 / 11.0
                return a
            }
        let expected = None
        Assert.AreEqual(expected, actual)

    [<TestMethod>]
    member this.FailReturnWithWrongAccuracy () =
        let actual = 
            round -3 {
                return 0.003
            }
        let expected = None
        Assert.AreEqual(expected, actual)


