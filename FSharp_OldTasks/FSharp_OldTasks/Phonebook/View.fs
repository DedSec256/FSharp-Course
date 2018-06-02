namespace Phonebook
open System

module View = 

    type IView = 
        abstract Print : string -> unit
        abstract Read : unit -> string

    type ConsoleView() = 
        interface IView with
            member this.Print text =
                Console.WriteLine(text)
            member this.Read() = 
                Console.ReadLine()

