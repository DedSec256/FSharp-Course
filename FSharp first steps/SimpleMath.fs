namespace FSharp_first_steps

module SimpleMath =

     (* Task 1.1 *)
     let (!) n : int = [2..n] |> List.reduce (*) 
    
     (* Task 1.4 *)
     (* TODO: more optimization *)
     let degreesOfTwoGenerator n m =
        let rec accGenerator m acc = 
            if (m < 0) then []
            else acc :: accGenerator (m - 1) (acc * 2)

        accGenerator m (pown 2 n)
        

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
        let matrixMuliply (A00, A01, A10, A11) (B00, B01, B10, B11) = 

            (*  A = (A00 A01), B = (B00 B01)  *)
            (*      (A10 A11)      (B10 B11)  *)

            ( (A00 * B00) + (A01 * B10), (A00 * B01) + (A01 * A11),
              (A10 * B00) + (A11 * B10), (A10 * B01) + (A11 * B11) )

        (* Binary pow *)
        let rec fastMatrixPow A n = 
            if   n = 1     then A
            elif n % 2 = 0 then 
                 let B =  fastMatrixPow A (n / 2)
                 matrixMuliply B B
            else matrixMuliply (fastMatrixPow A (n - 1)) A
            
        let (_, _, c, _) = fastMatrixPow (0, 1, 1, 1) n
        c
     
