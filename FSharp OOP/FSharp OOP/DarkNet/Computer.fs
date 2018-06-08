namespace FSharp_OOP

open System

module Computer = 
    
    /// <summary>
    /// Тип OS
    /// </summary>
    type OS = 
    | Windows
    | Linux
    | MacOS
    | ChromeOS
    | DOS
    | SunOS

    /// <summary>
    /// Интерфейс рандомизатора
    /// </summary>
    type IRandom =
        /// <summary>
        /// Получить следующее псевдослучайное число
        /// </summary>
        abstract member Random: unit -> int

    /// <summary>
    /// Базовая реализация рандома
    /// </summary>
    type StandartRandom(maxValue) = 
        interface IRandom with
            member this.Random() = System.Random().Next(maxValue)

    /// <summary>
    /// Модель компьютера
    /// </summary>
    type Computer(randomizer: IRandom, OS : OS) =
        let mutable isInfected = false

        (* Вероятность заражения, зависящая от ОС *)
        let criticalPossibility = 
            match OS with
            | Windows -> 60
            | Linux -> 15
            | MacOS -> 5
            | SunOS -> 50 //DeadOS :c
            | ChromeOS -> 80
            | DOS -> 1
            | _ -> raise (new ArgumentException("Invalid OS (or I forgot to add :D )"))

        (* ОС Компьютера. Ваш кэп. *)
        member this.OS
            with get() = OS

        (* Заражён ли компьютер. Ваш кэп. *)
        member this.IsInfected 
            with get() = isInfected
        
        /// <summary>
        /// Попытка заражения компьютера
        /// </summary>
        member this.TryToInfect() = 
            let possibility = randomizer.Random()
            isInfected <- (possibility <= criticalPossibility)
            isInfected

        /// <summary>
        /// Заразить компьютер
        /// </summary>
        member this.Infect() = 
            isInfected <- true
        
        override this.ToString() = 
            OS.ToString() + " | " + isInfected.ToString()
