namespace FSharp_first_steps

module SimpleMath =

    (* Task 1.1 *)
    let factorial n : int = [2..n] |> List.reduce (*) 
