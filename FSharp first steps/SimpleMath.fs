namespace FSharp_first_steps

module SimpleMath =

    (* Task 1.1 *)
    let factorial n : int = [2..n] |> List.reduce (*) 

    (* Task 1.4 *)
    let rec degreesOfTwoGenerator (n : float) (m : float) =
        if (m >= 0.0) then 
            degreesOfTwoGenerator n (m - 1.0) @ [2.0 ** (n + m)]
        else []

  