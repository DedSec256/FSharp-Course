namespace FSharp_OldTasks

module Validator = 

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