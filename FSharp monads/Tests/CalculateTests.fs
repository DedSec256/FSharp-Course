namespace FSharp_monads

open System
open Monads
open Microsoft.VisualStudio.TestTools.UnitTesting

[<TestClass>]
type CalculateTestClass () =

    [<TestMethod>]
    member this.SimpleCalculation () =
        let actual = 
            calculateI64 {
                let! x = "2"
                let! y = "2"
                let z = x + y
                return z
            }
        let expected = Some(4L)
        Assert.AreEqual(expected, actual)

    [<TestMethod>]
    member this.FailingParsing () =
        let actual = 
            calculateI64 {
                let! a = "12.12"
                let! b = "1"
                return a + b
            }
        let expected = None
        Assert.AreEqual(expected, actual)

    [<TestMethod>]
    member this.DivideByZero () =
        let actual = 
            calculateI64 {
                let! a = "12"
                let! b = "0"
                return a * 2L / b
            }
        let expected = None
        Assert.AreEqual(expected, actual)

    [<TestMethod>]
    member this.Calculations () =
        let actual = 
            calculateI64 {
                let! a = "1111"
                let! b = "3333"
                return (a + b) / 10000L
            }
        let expected = Some(0L)
        Assert.AreEqual(expected, actual)



