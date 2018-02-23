namespace FSharp_first_steps

module SimpleAlgorithms = 

    (* Task 2.1 *)
    let multiplyNumDigits x = 
        let rec multiplyAcc acc n = 
            if n % 10 = n then acc * n
            else multiplyAcc (acc * (n % 10)) (n / 10)

        multiplyAcc 1 x

    (* Task 2.2 *)
    let indexOf list x = 
        let rec getIndex list start = 
            match list with
            | []  ->          None
            | [a] ->
                if a = x then Some(start) 
                else          None
            | h :: t -> 
                if h = x then Some(start)
                else getIndex t (start + 1)

        getIndex list 0 

    (* Task 2.3 *)
    let isPalindrome s = 
        let rec areEqual (list1, list2) = 
            match (list1, list2) with
            | ([], [])              -> true
            | ([a], [b]) when a = b -> true
            | ([a], [b])            -> false
            | (h1 :: t1, h2 :: t2)  -> 
                if h1 <> h2 then false
                else areEqual (t1, t2)
            | (_,_)                 -> false

        let list = Seq.toList s
        areEqual (list, List.rev list) 
