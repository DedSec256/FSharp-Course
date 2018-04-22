namespace FSharp_async

open System.IO
open System.Net
open System.Text.RegularExpressions

module Downloader =
    /// <summary>
    /// Описывает результирующий статус загрузки
    /// </summary>
    type DownloadStatus<'a> = 
       |Done of 'a      (* Успешная загрузка:   'a - данные загрузки          *)
       |Error of string (* Ошибка при загрузке: string - информация об ошибке *)

    /// <summary>
    /// Возвращает размер страниц, на которые ссылается страница с url, включая её саму
    /// </summary>
    let downloadAsync(url : string) =

        (* Зашитое в сборку регулярное выражение для поиска ссылок *)
        let urlRegex = Regex("<a href\s*=\s*\"(https?://[^\"]+)\"\s*>", RegexOptions.Compiled)
		
        (* Асинхронно загружает страницу с _url *)
        let downloadPageAsync (_url : string) = 
            async {
                try
                   let request = WebRequest.Create(_url)
                   use! response = request.AsyncGetResponse()
                   use stream = response.GetResponseStream()
                   use reader = new StreamReader(stream)
                   let html = reader.ReadToEnd() 
                   return (_url, Done(html))
                with
                | error -> return (_url, Error(error.Message))
            }

        (* Получаем результат загрузки основной страницы *)
        let mainHtmlResult = downloadPageAsync(url) |> Async.RunSynchronously
        match mainHtmlResult with 
        | (_, Done(htmlDocument)) ->
                                (* Асинхронно загружаем все страницы, на которые ссылается основная *)
                                let matches = urlRegex.Matches(htmlDocument)
                                let tasks = [for _match in matches -> downloadPageAsync(_match.Groups.[1].Value)]
                                let results = Async.Parallel tasks |> Async.RunSynchronously
                                mainHtmlResult :: (results |> Array.toList)

        | _ -> [mainHtmlResult]

    /// <summary>
    /// Распечатывает результат работы функции downloadAsync
    /// </summary>
    let printDownloadData(url) =
        let results = downloadAsync(url)
        for result in results do
            match result with
            | (_url, Done(x)) -> printfn "[%s - %d symbols]" _url x.Length
            | (_url, Error(x)) -> printfn "[%s - error: %s]" _url x
