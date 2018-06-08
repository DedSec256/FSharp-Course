module Tests

open NUnit.Framework
open FsUnit
open NormalBetaReductionInterpreter

open Interpreter

[<TestFixture>]
module ``Interpreter tests`` = 
   let fvTestData () =
        [
            TestCaseData(%'x' << %'y').Returns(['x'; 'y'])
            TestCaseData('x' .< %'y').Returns(['y'])
        ]
   let substitutionTestData () =
        [
            TestCaseData(('y' .< %'x')^'x' <= ('s' .< %'y')).Returns('a' .< ('s' .< %'y'))
        ]

   let reduceTestData () =
        [
            TestCaseData(('x'.<('y' .< %'x')) << ('x' .< %'x')).Returns('y' .< ('x' .< %'x'))
            TestCaseData(('x' .< %'x') << ('x' .< %'x'), 'x' .< %'x' )
        ]
  
   [<TestCaseSource("fvTestData")>]
   let checkFvTests term = getFV term

   [<TestCaseSource("substitutionTestData")>]
   let checkSubstTests term = term

   [<Test>]
   let newNameTest () =
       let actual = getFreeName ('a' .< %'b') (%'a')
       actual |> should equal 'c'
