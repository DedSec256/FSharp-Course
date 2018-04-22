namespace FSharp_async

open System.IO
open System.Net
open System.Text.RegularExpressions

module Downloader =
    /// <summary>
    /// ��������� �������������� ������ ��������
    /// </summary>
    type DownloadStatus<'a> = 
       |Done of 'a      (* �������� ��������:   'a - ������ ��������          *)
       |Error of string (* ������ ��� ��������: string - ���������� �� ������ *)

    /// <summary>
    /// ���������� ������ �������, �� ������� ��������� �������� � url, ������� � ����
    /// </summary>
    let downloadAsync(url : string) =

        (* ������� � ������ ���������� ��������� ��� ������ ������ *)
        let urlRegex = Regex("<a href\s*=\s*\"(https?://[^\"]+)\"\s*>", RegexOptions.Compiled)
		
        (* ���������� ��������� �������� � _url *)
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

        (* �������� ��������� �������� �������� �������� *)
        let mainHtmlResult = downloadPageAsync(url) |> Async.RunSynchronously
        match mainHtmlResult with 
        | (_, Done(htmlDocument)) ->
                                (* ���������� ��������� ��� ��������, �� ������� ��������� �������� *)
                                let matches = urlRegex.Matches(htmlDocument)
                                let tasks = [for _match in matches -> downloadPageAsync(_match.Groups.[1].Value)]
                                let results = Async.Parallel tasks |> Async.RunSynchronously
                                mainHtmlResult :: (results |> Array.toList)

        | _ -> [mainHtmlResult]

    /// <summary>
    /// ������������� ��������� ������ ������� downloadAsync
    /// </summary>
    let printDownloadData(url) =
        let results = downloadAsync(url)
        for result in results do
            match result with
            | (_url, Done(x)) -> printfn "[%s - %d symbols]" _url x.Length
            | (_url, Error(x)) -> printfn "[%s - error: %s]" _url x

            
