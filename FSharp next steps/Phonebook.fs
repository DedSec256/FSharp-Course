module Phonebook

(* Task 5.3 *)
open System
open System.IO

type Record = 
    {
        Name : string
        Phone : string
    }
let separator = " - "
let toString (record : Record) = 
    record.Name + separator + record.Phone

let parseRecord (line : string) = 
    let newData = line.Split(separator)
    { Name = newData.[0]; Phone = newData.[1]}

(* Очень важная часть для соблюдения принципа OCP 
 * при масштабировании - вдруг мы захотим больше команд терминала? *)
type CallbackDelegate = delegate of List<Record> -> List<Record>
type Command = 
    {  
        Number : uint16
        Description : string
        Callback : CallbackDelegate
    }

let printCommands (commands : List<Command>) =
    commands |> List.iter(fun command -> printfn "%s" (command.Number.ToString() + ") " + command.Description))
    
let addRecord (records : List<Record>) name phone =
    { Name = name; Phone = phone } :: records

let rec findByName (records : List<Record>) name =
    match records with
    | [] -> 
        printfn "Не найдено: список записей пуст"
    | head :: tail ->
        if head.Name = name
            then printfn "%s" head.Phone
        (* Продолжаем поиск и находим всех, вдруг у нескольких совпадают имена? *)
        findByName tail name

let rec findByPhone (records : List<Record>) phone =
    match records with
    | [] -> 
        printfn "Не найдено: список записей пуст"
    | head :: tail ->
        if head.Phone = phone
            then printfn "%s" head.Name
        else findByPhone tail phone

let rec printAll (records : List<Record>) =
    match records with
    | [] -> ()
    | head :: tail -> printfn "%s" (head |> toString)
                      printAll tail

let saveToFile (records: list<Record>) path = 
    use writer = new StreamWriter(File.OpenWrite(path))
    List.iter (fun r -> writer.WriteLine((r |> toString))) records
    printfn "Данные успешно сохранены"    

let loadFromFile path =
    use reader = new StreamReader(File.OpenRead(path))
    let rec loadPhoneBook acc = 
        if (not reader.EndOfStream) then 
            let newRecord = parseRecord <| reader.ReadLine()
            loadPhoneBook (newRecord::acc)
        else
            printfn "Данные загружены"
            acc
    loadPhoneBook []

module TerminalCommands =
    let exitCommand : CallbackDelegate = new CallbackDelegate(fun records -> printfn "Выход..."
                                                                             exit 0)

    let newRecordCommand : CallbackDelegate = 
        new CallbackDelegate(fun records -> let newName = Console.ReadLine()
                                            printfn "Введите номер: "
                                            let newPhone = Console.ReadLine()
                                            addRecord records newName newPhone)

    let findbyNameCommand: CallbackDelegate =
        new CallbackDelegate(fun records -> printfn "Введите имя для поиска: "
                                            let name = Console.ReadLine()
                                            findByName records name
                                            records)

    let findbyPhoneCommand: CallbackDelegate =
        new CallbackDelegate(fun records -> printfn "Введите телефон для поиска: "
                                            let phone = Console.ReadLine()
                                            findByPhone records phone
                                            records)

    let printAllRecordsCommand: CallbackDelegate = 
        new CallbackDelegate(fun records -> printAll records
                                            records)

    let saveToFileCommand: CallbackDelegate = 
        new CallbackDelegate(fun records -> printfn "Введите конечный путь к файлу: "
                                            let path = Console.ReadLine()
                                            try
                                            saveToFile records path
                                            records
                                            with
                                               | _ -> 
                                                      printfn "Не удалось сохранить данные"
                                                      records)

    let loadFromFileCommand: CallbackDelegate = 
        new CallbackDelegate(fun records -> printfn "Введите путь к исходному файлу: "
                                            let path = Console.ReadLine()
                                            try
                                               let loaded = loadFromFile path
                                               loaded
                                            with
                                               | :? FileNotFoundException ->
                                                     printfn "Не удалось загрузить данные: файл не найден"
                                                     records
                                               | _ -> 
                                                    printfn "Не удалось загрузить данные: ошибка при считывании данных"
                                                    records)

    (* Инициализируем команды терминала *)
    let Commands = [{ Number = 1us; Description = "Выход"; Callback = exitCommand }]
    (* ...таким же образом добавляем остальные... *)

    let rec phonebookUserInterface (records : List<Record>) =

        printCommands Commands
        let input = Console.ReadLine()
        let success, result = System.Int32.TryParse(input)

        if (not success || result < 1 || result > Commands.Length) then
            printfn "Неверный ввод. Выберите команду:"
            phonebookUserInterface records
        else
            (* Было бы намного лучше, если бы команды с их номерами хранились в словаре, но нельзя :c *)
            (* Поэтому линейный поиск :c *)
            let command = List.find(fun com -> com.Number = (uint16)result) Commands
            phonebookUserInterface (command.Callback.Invoke records)
