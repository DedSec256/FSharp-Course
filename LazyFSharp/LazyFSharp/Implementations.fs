module Implementations

open System
open System.Threading
open Interface
open Unchecked
open System.Diagnostics
 
[<AbstractClass>]
type LazyBase<'a>() =
    member val isValueCreated = false with get, set (* Да, они public....а protected нет...*)
    member val value =  defaultof<'a> with get, set (* Что делать в данном случае? *)
                                                    (* И я не хочу одни и те же поля делать ctrl c + ctrl v 
                                                       приватными в каждом наследнике...
                                                       Убирать хотелку? :c *)
    abstract member Get : unit -> 'a

    interface ILazy<'a> with
        member this.Get() = this.Get() (* Какой забавный костыль в F# 
                                          для переопределения интерфейса из наследников *)

/// <summary>
/// Lazy для одного потока
/// </summary>
/// <param name="supplier">Инициализатор значения</param>   
type SingleThreadedLazy<'a>(supplier : unit -> 'a) =  
    inherit LazyBase<'a>()

    override this.Get() = 
        if  not this.isValueCreated 
        then 
            this.value          <- supplier()
            this.isValueCreated <- true
        this.value
                 
/// <summary>
/// Lazy для многопоточного доступа
/// </summary>
/// <param name="supplier">Инициализатор значения</param>                    
type MultiThreadedLazy<'a> (supplier : unit -> 'a) =
    inherit LazyBase<'a>()

    let lockObj = new Object()

    override this.Get() =
        if Interlocked.Equals(this.isValueCreated, false)
            then lock(lockObj) (fun () -> 
                                this.value          <- supplier()
                                this.isValueCreated <- true)                                
        this.value
    
/// <summary>
/// Lock-free Lazy
/// </summary>
/// <param name="supplier">Инициализатор значения</param>             
type LockFreeLazy<'a when 'a : equality and 'a : not struct> (supplier : unit -> 'a) =
        inherit LazyBase<'a>() 

        override this.Get() = 
            let rec get() = 
                let currentValue  = this.value
                let computedValue = supplier()
            
                if Interlocked.CompareExchange(&this.value, computedValue, currentValue) <> currentValue
                then 
                     Thread.SpinWait(15)
                     get()
                else this.isValueCreated <- true
                     computedValue
            get()
                