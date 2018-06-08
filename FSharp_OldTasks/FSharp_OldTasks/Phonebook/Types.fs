namespace Phonebook

module Types = 
    open System.Collections.Generic
    open Phonebook.View

    /// <summary>
    /// Запись в телефонной книге
    /// </summary>
    type Record = 
        {
            Name : string
            Phone : string
        }
    with 
        override this.ToString() = 
            this.Name + " - " + this.Phone

        static member parse(line : string) = 
            let newData = line.Split(" - ")
            {Name = newData.[0]; Phone = newData.[1]}

    /// <summary>
    /// Результат обработки команды
    /// </summary>
    type ProcessResult = 
         | Done of string
         | Failed of string
    with 
        override this.ToString() = 
           match this with 
            |Done(result) -> result
            |Failed(m) -> m

    /// <summary>
    /// Тип обработчика команд в телефонной книге
    /// </summary>
    type CallbackDelegate = delegate of IView * list<Record> -> ProcessResult * list<Record>
    type Command(desc : string, callback : CallbackDelegate) = 
           member this.Description = desc
           member this.Callback = callback
    with 
        override this.ToString() =
            this.Description

    /// <summary>
    /// Телефонная книга
    /// </summary>
    type Phonebook(view : IView) =
        let mutable Commands = new Dictionary<int, Command>()

        member this.AddCommand (command : Command) = 
            Commands.Add(Commands.Count + 1, command)

        member this.printCommands() =
            for command in Commands do
                view.Print(command.Key.ToString() + ") " + command.Value.ToString())

        member this.phonebookUserInterface (records : list<Record>) =
            this.printCommands()
            let input = view.Read()
            let success, result = System.Int32.TryParse(input)

            if (not success || result < 1 || result > Commands.Count) then
                view.Print("Неверный ввод. Выберите команду:")
                this.phonebookUserInterface records
            else
                let command = Commands.Item(result)
                let (result, newRecords) = command.Callback.Invoke(view, records)
                view.Print(result.ToString())
                this.phonebookUserInterface newRecords

               