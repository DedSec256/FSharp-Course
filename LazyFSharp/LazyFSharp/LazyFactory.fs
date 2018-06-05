module LazyFactory

open Interface
open Implementations

type LazyFactory() =
    
    static member CreateSingleThreadedLazy<'a>(supplier : unit -> 'a) =
        new SingleThreadedLazy<'a>(supplier) :> LazyBase<'a>
            
    static member CreateMultiThreadedLazy<'a>(supplier : unit -> 'a) =
        new MultiThreadedLazy<'a>(supplier) :> LazyBase<'a>
            
    static member CreateLockFreeLazy<'a when 'a : not struct and 'a : equality>(supplier : unit -> 'a) =
        new LockFreeLazy<'a>(supplier) :> LazyBase<'a>
