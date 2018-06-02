module Tests

open System
open NUnit.Framework
open FsUnit
open NUnit.Framework
open FsUnit
open FSharp_OldTasks
open Validator

[<TestFixture>]
module ``Validator tests`` = 
   let testData () =
        [
            TestCaseData("(())").Returns(true)
            TestCaseData("((AA)b%)").Returns(true)
            TestCaseData("{{}[]]").Returns(false)
            TestCaseData("abc").Returns(true)
            TestCaseData("").Returns(true)
            TestCaseData("{aa((a{{dd}]]").Returns(false)
            TestCaseData("{([])}").Returns(true)
            TestCaseData("(").Returns(false)
            TestCaseData("({aaaa}bbbbbBB&^&)[dd^$#]").Returns(true)
        ]
  
   [<TestCaseSource("testData")>]
   let checkTests str = 
        validate str
