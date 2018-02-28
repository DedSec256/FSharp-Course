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

    (* Task 2.4 *)
    let rec mergeSort list = 

        let splitList n list = 
            let rec splitAcc n cont = function
            | []             -> cont([], [])
            | l when n = 0   -> cont([], l)
            | h :: t -> splitAcc (n - 1) (fun acc -> cont(h :: fst acc, snd acc)) t

            splitAcc n id list

        let merge left right = 
            let rec mergeRec left right cont =
                match (left, right) with
                | (l, []) -> cont l
                | ([], r) -> cont r
                | (h1 :: t1, h2 :: t2) -> 
                    if h1 <= h2 then mergeRec t1 right (fun acc -> cont(h1 :: acc))
                    else             mergeRec left t2  (fun acc -> cont(h2 :: acc))

            mergeRec left right id

        match (List.length list) with
        | l when l <= 1 -> list
        | len           ->  
            let left, right = list |> splitList (len / 2) 
            merge (mergeSort left) (mergeSort right)
    

    


    
