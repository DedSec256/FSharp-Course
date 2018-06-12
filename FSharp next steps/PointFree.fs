module PointFree

    (* Task 5.2 *)
    let func x l = List.map (fun y -> y * x) l

    let func'1 x: int list -> int list = 
        List.map (fun y -> y * x)

    let func'2 x: int list -> int list = 
        List.map (fun y -> (*) x y)

    let func'3 x: int list -> int list = 
        List.map ((*) x)
    
    let func'4: int -> int list -> int list = 
        List.map << (*)
    
    let funcRelease = 
        List.map << (*)

   
    module tests =
        open NUnit.Framework
        open FsCheck

        [<Test>]
        let ``check base func and funcRelease for equality`` () =
            Check.Quick (fun x l -> func x l = funcRelease x l)