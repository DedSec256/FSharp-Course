module Monads

    open System

    (* Task 7.1 *)
    /// <summary>
    /// Makes computations with a given accuracy
    /// </summary>
    type RoundingBuilder(accuracy : int) =

        member this.Bind (x: float, f) =
            try
                f (Math.Round(x, accuracy))
            with 
            | _ -> None // if wrong accuracy, for example 

        member this.Return (x: float) =
            try
                Some (System.Math.Round(x, accuracy))
            with
            | _ -> None // if wrong accuracy, for example 


    /// <summary>
    /// Convenient definition for RoundingBuilder
    /// </summary>
    let round = RoundingBuilder

