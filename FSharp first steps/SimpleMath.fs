namespace FSharp_first_steps

module SimpleMath =

    (* Task 1.1 *)
    let factorial n : int = [2..n] |> List.reduce (*) 

    (* Task 1.4 *)
    let degreesOfTwoGenerator n m = 
    [
        for i in n..n+m
        -> 2.0 ** i
    ]

