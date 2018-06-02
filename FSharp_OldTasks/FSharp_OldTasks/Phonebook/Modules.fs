module Phonebook.CommandModules

open Phonebook.Types
open System.IO
open System.Reflection
open System.Text

[<AbstractClass>]
type CommandsModule() = 

     abstract member AddCommandsInPhonebook : Phonebook -> CommandsModule

     (* Рефлексией проходится по всем командам-property в классе, автоматически добавляем их в книгу *)
     default this.AddCommandsInPhonebook phonebook = 
             this.GetType().GetProperties()
                |> Array.filter(fun field -> field.GetValue(this).GetType() = typeof<Command>)
                |> Array.iter(fun command -> phonebook.AddCommand(command.GetValue(this) :?> Command))
             this

type StandartCommandsModule() =  
    inherit CommandsModule()

    member this.exitCommand = 
        Command(
            "Закрыть книгу", 
                 new CallbackDelegate(fun view -> 
                                      fun records -> view.Print("Выход...")
                                                     exit 0)
        )
    member this.newRecordCommand = 
        Command("Добавить запись",
                 new CallbackDelegate(fun view ->
                                      fun records -> 
                                          view.Print("Введите Имя: ")
                                          let newName = view.Read()
                                          view.Print("Введите Номер: ")
                                          let newPhone = view.Read()
                                          (Done("Запись успешно добавлена"), { Name = newName; Phone = newPhone } :: records))
    )

    member this.findbyNameCommand = 
        Command("Найти запись по имени",
                 new CallbackDelegate(fun view -> 
                                      fun records -> 

                                          view.Print("Введите имя для поиска: ")
                                          let name = view.Read()
                                          let result = StringBuilder()

                                          records |> List.filter(fun r -> r.Name = name)
                                                  |> List.iter(fun r -> result.AppendLine(r.ToString()) |> ignore)
                                          let resStr = result.ToString()  
                                          if System.String.IsNullOrEmpty(resStr)
                                            then (Failed("Не найдено ни одной записи"), records)
                                          else (Done(resStr), records)
                 )
    )

    member this.findbyPhoneCommand = 
        Command("Найти запись по телефону",
                 new CallbackDelegate(fun view -> 
                                      fun records -> 

                                          view.Print("Введите телефон для поиска: ")
                                          let phone = view.Read()
                                          let result = StringBuilder()

                                          records |> List.filter(fun r -> r.Phone = phone)
                                                  |> List.iter(fun r -> result.AppendLine(r.ToString()) |> ignore)
                                          let resStr = result.ToString()  
                                          if System.String.IsNullOrEmpty(resStr)
                                            then (Failed("Не найдено ни одной записи"), records)
                                          else (Done(resStr), records)
                 )
        )

    member this.saveToFileCommand = 
        Command("Сохранить записи в файл",
                 new CallbackDelegate(fun view -> 
                                      fun records -> 
                                          view.Print("Введите конечный путь к файлу: ")
                                          let path = view.Read()
                                          try
                                            use writer = new StreamWriter(File.OpenWrite(path))
                                            records |> List.iter (fun r -> writer.WriteLine((r.ToString())))
                                            (Done("Данные успешно сохранены"), records)
                                          with
                                            | _ -> 
                                                (Failed("Не удалось сохранить данные"), records)
                 )
        )

    member this.loadFromFileCommand = 
        Command("Загрузить записи из файла",
                 new CallbackDelegate(fun view -> 
                                      fun records -> 
                                          let loadFromPath path =
                                            use reader = new StreamReader(File.OpenRead(path))
                                            let rec loadPhoneBook acc = 
                                                if (not reader.EndOfStream) then 
                                                    let newRecord = Record.parse(reader.ReadLine())
                                                    loadPhoneBook (newRecord::acc)
                                                else
                                                    acc
                                            loadPhoneBook []

                                          view.Print("Введите путь к исходному файлу: ")
                                          let path = view.Read()
                                          try
                                            let loaded = loadFromPath path
                                            (Done("Данные успешно загружены"), loaded)
                                          with
                                            | :? FileNotFoundException ->
                                                 (Failed("Не удалось загрузить данные: файл не найден"), records)
                                            | _ -> 
                                                 (Failed("Не удалось загрузить данные: ошибка при считывании данных"), records)
                 )
        )

