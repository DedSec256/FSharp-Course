open FSharp_second_steps.SimpleDataStructures
open FSharp_second_steps
open NUnit.Framework
open FsUnit
open FsCheck
open System

module Tests = 


        (* Task 3.1 tests *)
        (* Проверяем на корректность countEvenFilter *)
        [<TestFixture>]
        module ``Count even tests`` =
            let testData () =
                [
                    TestCaseData([]).Returns(0)
                    TestCaseData([1; 2; 3; -4; 0]).Returns(3)
                    TestCaseData([8; 10; 12; -4; 0]).Returns(5)
                ]
            [<TestCaseSource("testData")>]
            let checkTests list = 
                countEvenFilter list

        (* Проверяем эквивалентность всех функций *)
        [<Test>]
        let ``check equals`` () =
            Check.Quick (fun list -> countEvenFilter list = countEvenFold list && 
                                     countEvenFilter list = countEvenMap list)
 

        (* Task 3.2 tests *)
        [<Test>]
        let ``map for empty tree``() =
            map Tree.Null id |> should equal Tree.Null

        [<Test>]
        let ``square a tree with only root``() =
            map (Tree(2, Tree.Null, Tree.Null)) (fun x -> x * x) |> 
            should equal (Tree(4, Tree.Null, Tree.Null))

        [<Test>]
        let ``square a tree ``() =
            map (Tree(2, 
                      Tree(2, Tree.Null, Tree.Null), 
                      Tree.Null)
                 ) (fun x -> x * x) |> 
            should equal 
                (Tree(4, 
                      Tree(4, Tree.Null, Tree.Null), 
                      Tree.Null))

        (* Task 3.3 tests *)
        [<Test>]
        let ``calculate empty tree must throw ArgumentException`` () =
            calculate None |> 
            should throw typeof<System.ArgumentException>

        [<Test>]
        let ``test for value`` () = 
            calculate (Some(Value(2))) |> should equal 2
    
        [<Test>]
        let ``1/0 must throw DivideByZeroException`` ()=
            calculate (Some(Div(Value 1, Value 0))) |>
            should throw typeof<System.DivideByZeroException>

        [<Test>]
        let ``test for all operations (5 - 10) * 2 + 10 should equal 0`` () =
            calculate (Some(Add(Mult(Sub(Value 5, Value -10), Value 2), Value 10))) |> should equal 0

        (* Task 3.4 test *)
        [<Test>]
        let `` 10 first prime numbers ``() =
            generatePrimeSeq() |> Seq.toList |> should equal [2, 3, 5, 7, 11, 13, 17, 19, 23, 29] 
