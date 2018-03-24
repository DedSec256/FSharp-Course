namespace FSharp_next_steps

module validator = 

    (* Task 5.1 *)
    type Brackets = 
        {
            Open : char
            Close : char
        }

    (* А вдруг число пар скобочек увеличится! Добавим хоть какую-нибудь возможность масштабирования *)
    let bracketFactory bracket = 
        match bracket with
        | ')' | '(' -> Some({ Open = '('; Close = ')' })
        | ']' | '[' -> Some({ Open = '['; Close = ']' })
        | '}' | '{' -> Some({ Open = '{'; Close = '}' })
        | _ -> None


    let validate (s: string) =
        let chars = s.ToCharArray() |> List.ofArray

        let rec validateRec chars buffer =
            match chars with
            | [] -> List.empty = buffer
            | head :: tail -> match bracketFactory head with
                                | Some(x) when head = x.Open -> validateRec tail (head :: buffer)
                                | Some(x) -> if buffer.Head = x.Open then validateRec tail buffer.Tail else false
                                | None -> validateRec tail buffer
        validateRec chars []

(* 
   Датасет для тестов взят у Тучиной Анастасии: 
   https://github.com/AnastasiaTuchina/FSharp-Projects/blob/hw5/hw5/task5_1/task5_1/Tests.fs
 *)
module tests =
    open NUnit.Framework
    open FsUnit

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
        validator.validate str
