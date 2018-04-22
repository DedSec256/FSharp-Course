namespace FSharp_OOP

open System.Text
open System.Collections
open System.Collections.Generic

module BST =

    /// <summary>
    /// ������ ������
    /// </summary>
    type Node<'a> =
        | Node of 'a * Node<'a> * Node<'a>
        | Empty 
        with
            override this.ToString() = 
                (* ��������� �������� ��������� ��� �������� ��������� *)
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
    /// �������� ������ ������ ��� ������������ �����
    /// </summary>
    type Tree<'a when 'a: comparison>() =

        (* ������ ������. ��� ���. *)
        let mutable root = Empty 
        (* ������ ������. ��� ���. *)
        let mutable size = 0

        member this.Size 
            with get() = size

        member this.IsEmpty 
            with get() = root = Empty

        override this.ToString() = 
            root.ToString()
       
        /// <summary>
        /// ������� ������. ��� ���
        /// </summary>
        member this.Clear() = 
            root <- Empty
            size <- 0
        
        interface IEnumerable<'a> with        
            (* Enumerator ��� ����������������� ������ ������ (���������������� �� �����������) *)
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

            (* Enumerator, ������� ����� �� ���������� :/ *)
            member this.GetEnumerator(): IEnumerator = 
               (this :> IEnumerable<'a>).GetEnumerator() :> IEnumerator
       
        /// <summary>
        /// �������� �������� � ������. ��� ���.
        /// ������������ ������� ��������� Tree.Add(123).Add(122)...
        /// </summary>
        member this.Add(data) = 
            let rec recAdd data startNode = 
                match startNode with
                | Empty -> Node(data, Empty, Empty)
                | Node(x, l, r) -> match data with  
                                   | data when data < x -> Node(x, recAdd data l, r)
                                   | data when data > x -> Node(x, l, recAdd data r)
                                   | _ -> size <- size - 1 (* ���� ����� ������� ��� ���������� *)
                                          Node(x, l, r)
            size <- size + 1
            root <- recAdd data root
            this

        /// <summary>
        /// ���� �� ������� �� ��������� data � ������. ��� ���.
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
        /// ���� �� ������� �� ��������� data � ������. ��� ���.
        /// ������������ ������� ��������� Tree.Remove(123).Remove(122)...
        /// </summary>
        member this.Remove(data) = 

            (* �������� ����� ����� ������ � ������/���������, ����� ���������� � node *)
            let rec recGetMin (node: Node<'a>) = 
                match node with
                | Node(_, l, _) -> match l with
                                   | Empty -> node
                                   | _ -> recGetMin l
                | x -> x 

            let rec recRemove data node = 
                match node with 
                | Empty -> size <- size + 1 (* ���������� size <- size - 1, ���� ������ ������ *)
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
            


    
