﻿namespace FSharp_second_steps

open System

module SimpleDataStructures = 

    (* Task 3.3 *)
    type Operation =
        | Value of int 
        | Add of Operation * Operation
        | Sub of Operation * Operation
        | Mult of Operation * Operation
        | Div of Operation * Operation
    
    let rec calculate (tree : Operation option) =
        match tree with
        | None -> raise (System.ArgumentException "Nothing to calculate")
        | Some(t) ->
            match t with
            | Value(x) -> x
            | Mult(x, y) -> calculate(Some(x)) * calculate(Some(y))
            | Div(x, y) -> if (calculate (Some(y))) <> 0
                           then calculate(Some(x)) / calculate(Some(y))
                           else raise (System.DivideByZeroException "Wrong operation: cannot divide by zero")
            | Add(x, y) -> calculate(Some(x)) + calculate(Some(y))
            | Sub(x, y) -> calculate(Some(x)) - calculate(Some(y))

    (* Task 3.4 *)
    let generatePrimeSeq() = 
        let isPrime n =
            not ({2..n - 1} |> Seq.exists(fun x -> n % x = 0))

        Seq.initInfinite(fun x -> x + 2) |> Seq.filter isPrime

    (* Task 3.2 *)
    type Tree<'a> = 
        | Null
        | Tree of 'a * Tree<'a> * Tree<'a>
    
    let rec map tree f =
        match tree with
        | Null -> Null
        | Tree(root, left, right) -> 
            Tree(f root, map left f, map right f)

    (* Task 3.1 *)
    (* --- with fold ---*)
    let countEvenFold =
        List.fold(fun acc x -> if (x % 2 = 0) then acc + 1 else acc) 0

    (*--- with filter ---*) 
    let countEvenFilter =
        List.filter(fun x -> x % 2 = 0) >> List.length

    (*--- with map ---*)
    let countEvenMap =
        List.map(fun x -> (x % 2) ^^^ 1 ) >> List.sum
