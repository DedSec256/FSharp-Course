namespace FSharp_OOP

open System.Text
open System.Collections
open System.Collections.Generic

module BST =

    /// <summary>
    /// Ячейка дерева
    /// </summary>
    type Node<'a> =
        | Node of 'a * Node<'a> * Node<'a>
        | Empty 
        with
            override this.ToString() = 
                (* Добавляет красивую табуляцию для красивой отрисовки *)
                let tab(n) =
                    let sB = new StringBuilder(n + 1)
                    sB.Append('\t', n).ToString()

                let rec recToString node n = 
                    match node with
                    | Node(x, l, r) -> "(" + x.ToString() + ") ->\n" 
                                       + tab(n) + recToString l (n + 2) + "\n"
                                       + tab(n) + recToString r (n + 2)
                    | Empty -> "(null)"
                recToString this 2

    /// <summary>
    /// Двоичное дерево поиска для сравниваемых типов
    /// </summary>
    type Tree<'a when 'a: comparison>() =

        (* Корень дерева. Ваш кэп. *)
        let mutable root = Empty 
        (* Размер дерева. Ваш кэп. *)
        let mutable size = 0

        member this.Size 
            with get() = size

        member this.IsEmpty 
            with get() = root = Empty

        override this.ToString() = 
            root.ToString()
       
        /// <summary>
        /// Очищает дерево. Ваш кэп
        /// </summary>
        member this.Clear() = 
            root <- Empty
            size <- 0
        
        interface IEnumerable<'a> with        
            (* Enumerator для последовательного обхода дерева (отсортированного по возрастанию) *)
            member this.GetEnumerator() =               
                let rec enumerateRec node =
                    seq {
                        match node with
                        | Node(x, Empty, Empty) -> yield x
                        | Node(x, left, right) ->
                                yield! enumerateRec left
                                yield x
                                yield! enumerateRec right
                        | Empty -> ignore
                    }
                (enumerateRec root).GetEnumerator()

            (* Enumerator, которым никто не пользуется :/ *)
            member this.GetEnumerator(): IEnumerator = 
               (this :> IEnumerable<'a>).GetEnumerator() :> IEnumerator
       
        /// <summary>
        /// Добавить значение в дерево. Ваш кэп.
        /// Поддерживает текучий синтаксис Tree.Add(123).Add(122)...
        /// </summary>
        member this.Add(data) = 
            let rec recAdd data startNode = 
                match startNode with
                | Empty -> Node(data, Empty, Empty)
                | Node(x, l, r) -> match data with  
                                   | data when data < x -> Node(x, recAdd data l, r)
                                   | data when data > x -> Node(x, l, recAdd data r)
                                   | _ -> size <- size - 1 (* Если такой элемент уже существует *)
                                          Node(x, l, r)
            size <- size + 1
            root <- recAdd data root
            this

        /// <summary>
        /// Есть ли элемент со значением data в дереве. Ваш кэп.
        /// </summary>
        member this.Contains(data) =
            let rec recContains data startNode =
                match startNode with
                | Empty -> false
                | Node(x, l, r) -> match data with  
                                   | data when data < x -> recContains data l
                                   | data when data > x -> recContains data r
                                   | _ -> true
            recContains data root

        /// <summary>
        /// Есть ли элемент со значением data в дереве. Ваш кэп.
        /// Поддерживает текучий синтаксис Tree.Remove(123).Remove(122)...
        /// </summary>
        member this.Remove(data) = 

            (* Получает самую левую ячейку в дереве/поддереве, поиск начинается с node *)
            let rec recGetMin (node: Node<'a>) = 
                match node with
                | Node(_, l, _) -> match l with
                                   | Empty -> node
                                   | _ -> recGetMin l
                | x -> x 

            let rec recRemove data node = 
                match node with 
                | Empty -> size <- size + 1 (* Нивелируем size <- size - 1, если дерево пустое *)
                           Empty
                | Node(x, l, r) -> match data with
                                   | data when data < x -> Node(x, recRemove data l, r)
                                   | data when data > x -> Node(x, l, recRemove data r)
                                   | _ -> match r with
                                          | Empty -> l
                                          | Node(y, Empty, rr) -> Node(y, l, rr)
                                          | Node(y, ll, rr) -> let min = recGetMin ll
                                                               match min with
                                                               | Node(a, _, _) -> Node(a, l, recRemove a r)
                                                               | Empty -> Empty
            size <- size - 1
            root <- recRemove data root
            this
            


    
