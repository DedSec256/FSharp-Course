namespace FSharp_first_steps

module SimpleMath =

    (* Task 1.1 *)
    let (!) n : int = [2..n] |> List.reduce (*) 
    
    (* Task 1.4 *)
    (* TODO: optimization *)
    let rec degreesOfTwoGenerator (n : float) (m : float) =
        if (m < 0.0) then []
        else degreesOfTwoGenerator n (m - 1.0) @ [2.0 ** (n + m)]

    (* Task 1.3 *)
    let reverse list = 
        let rec reverseSignature acc list =
            match list with
            | [] -> acc
            | [x] -> x::acc
            | head::tail -> reverseSignature (head::acc) tail 

        reverseSignature [] list
        