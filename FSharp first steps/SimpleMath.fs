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
     

    (* Task 1.2, modified - difficulty O(ln n) 
     * ---------------------------------------------
     * Introduction:
     * ---------------------------------------------
     * We know that (F0 F1) * P^n = (Fn Fn+1), 
     * where P = (0 1) and F0 = 0, F1 = 1
     *           (1 1)
     *
     * => P^n = (a b) => (F0 F1) * (a b) = (Fn Fn+1)
     *          (c d)              (c d)
     * => Fn = F0 * a + F1 * c = c
     *
     * ---------------------------------------------
     * Action plan:
     * ---------------------------------------------
     * Find P^n => get c => find Fn 😏
     *) 

     let fibonacci n = 

        (* Standart square matrix multiply *)
        let matrixMuliply ((A : int list list), (B : int list list)) = 
            [
                [ ( (A.Item 0).Item 0 * (B.Item 0).Item 0 ) + ( (A.Item 0).Item 1 * (B.Item 1).Item 0 );
                  ( (A.Item 0).Item 0 * (B.Item 0).Item 1 ) + ( (A.Item 0).Item 1 * (B.Item 1).Item 1 ) ];
                [ ( (A.Item 1).Item 0 * (B.Item 0).Item 0 ) + ( (A.Item 1).Item 1 * (B.Item 1).Item 0 );
                  ( (A.Item 1).Item 0 * (B.Item 0).Item 1 ) + ( (A.Item 1).Item 1 * (B.Item 1).Item 1 ) ];
            ]

        (* Binary pow *)
        let rec fastMatrixPow (A : int list list) n =
            if   n = 1     then A
            elif n % 2 = 0 then matrixMuliply (fastMatrixPow A (n / 2), fastMatrixPow A (n / 2))
            else matrixMuliply (fastMatrixPow A (n - 1), A)
        
        ((fastMatrixPow [[0; 1];[1;1]] n).Item 1).Item 0
        
        
     
