namespace NormalBetaReductionInterpreter

module Interpreter = 

    type LambdaTerm =
    | Var         of char
    | Application of LambdaTerm * LambdaTerm
    | Abstraction of char       * LambdaTerm
    
    let inline (~%) (x : char      )                  = Var(x)
    let inline (<<) (A : LambdaTerm) (B : LambdaTerm) = Application(A, B)
    let inline (.<) (x : char      ) (A : LambdaTerm) = Abstraction(x, A)
    let inline (^)  (A : LambdaTerm) (x : char      ) = (A, x)

    (* Получаем имена свободных переменных *)
    let getFV term =
        let rec getRecFV t vars =
            match t with
            | Var x             -> x :: vars
            | Application(S, T) -> (getRecFV S (getRecFV T vars))
            | Abstraction(x, S) -> (getRecFV S vars) |> List.filter(fun v -> v <> x)
        getRecFV term []

    let ruAlphabet = Set(['а'..'я'])
    let enAlphabet = Set(['a'..'z'])
    (* Получаем свободное имя, не конфликтующее ни в A, ни в B *)
    let getFreeName A B = 
        let allFV = (getFV A) @ (getFV B) |> Set.ofList
        let freeNames = (enAlphabet - allFV) |> Set.toList
        if  freeNames = list.Empty then
            (* Как говорил Билл Гейтс: "Букв из двух алфавитов хватит на всё!" *)
            (ruAlphabet - allFV) |> Set.toList |> List.head 
        else freeNames |> List.head
    
    (* Заменяем x из A на B *)
    let rec (<=) (A, x) B =
        let rec substitute A x B =
            match A with
            | Var var             -> if var = x then B else A
            | Application(T1, T2) -> (T1^x <= B) << (T2^x <= B)
            | Abstraction(var, T) -> 
                match B with
                | Var(_) when x = var -> T
                | _ when (not (((getFV B) |> List.contains var) 
                            && ((getFV T) |> List.contains x))) -> 
                    var .< (T^x <= B)
                | _ -> 
                    let freeVar = getFreeName B T
                    freeVar .< (T^x <= B)
        substitute A x B
    
    (* Сама бета-редукция *)
    let rec (~&) A = 
        let rec betaReduce term =
            match term with
            | Var(_) as var -> var
            | Application(A, B) ->
                match A with
                | Abstraction(var, A2) -> &(A2^var <= B)
                | _ -> &A << &B
            | Abstraction(x, C) -> x .< &C
        betaReduce A