namespace FSharp_monads
open System

module Monads = 

    (* Task 7.2 *)
    /// <summary>
    /// Makes computations with 'a
    /// </summary>
    type CalculateBuilder<'a>(converter : string -> 'a option) = 
   
        member this.Bind(x: string, f) =
            match converter(x) with
            | None -> None
            | Some(a) -> try 
                            f a
                         with 
                         | _ -> None

        member this.Return(x: 'a) =
            Some(x)

    /// <summary>
    /// Option converter from string to Int64 option
    /// </summary>
    let convertToInt (str : string) =
        let success, value =  Int64.TryParse(str)
        match success with 
        | true -> Some(value)
        | _ -> None 
        
    /// <summary>
    /// Convenient definition for CalculateBuilder<Int64>
    /// </summary>
    let calculateI64 = new CalculateBuilder<Int64>(convertToInt)


