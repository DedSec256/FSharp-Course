namespace FSharp_OOP

open System
open System.Text
open FSharp_OOP.Computer

module DarkNet = 
 
    /// <summary>
    /// Обьект игры
    /// winPredicate - условие окончания игры
    /// computers - список компьютеров
    /// matrix - матрица смежности компьютеров
    /// </summary>
    type Network(winPredicate : Network -> bool, 
                 computers: list<Computer>, 
                 matrix: list<list<bool>>) =
        
        (* Индексы заражённых в списке *)
        let mutable infected = List.Empty

        (* Следующий шаг в игре *)
        let Step() = 
            (* Индексы инфицированных на данном шаге *)
            let mutable infectedInThisStep = List.Empty

            (* Пытаемся распространить заражение *)
            infected |> List.iter(
                fun infectedIndex ->
                    for j in [0..computers.Length - 1] do
                        let computer = computers.[j]
                        if (not computer.IsInfected && matrix.[infectedIndex].[j])
                            then 
                                if(computer.TryToInfect()) then 
                                    infectedInThisStep <- j :: infectedInThisStep)
            (* Добавляем индексы всех инфицированных на данном шаге *)
            infectedInThisStep |> List.iter(fun i -> infected <- i :: infected) 

        (* Проверяем, есть ли в списке компьютеров хоть один заражённый, и добавляем его индекс *)
        do 
            infected <- (computers |> List.findIndex(fun c -> c.IsInfected)) :: infected
            (* Можно также проверить размерность матрицы смежности *)
            ()

        /// <summary>
        /// Возвращает последовательность всех ходов в игре
        /// </summary>
        member this.Play() = 
            [
                while not (winPredicate(this)) do 
                    Step()
                    yield this.ToString() (* Отдаём состояние игры после каждого хода *)
            ]

        /// <summary>
        /// Список всех здоровых компьютеров в сети
        /// </summary>
        member this.Uninfected 
            with get() = computers |> List.where(fun c -> not c.IsInfected)
            (* По-хорошему, нужно отдавать копии компьютеров, а не ссылки, дабы не допустить изменения извне...*)

        /// <summary>
        /// Список всех заражённых компьютеров в сети
        /// </summary>
        member this.Infected 
            with get() = infected |> List.map(fun i -> computers.[i])
            (* По-хорошему, нужно отдавать копии компьютеров, а не ссылки, дабы не допустить изменения извне...*)
    
        override this.ToString() = 
            let sB = new StringBuilder()
            computers |> List.iter(fun c -> sB.AppendLine(c.ToString()) |> ignore)
            sB.ToString()
