namespace Tests

open System
open FSharp_async.Downloader
open Microsoft.VisualStudio.TestTools.UnitTesting

[<TestClass>]
type TestClass () =

    [<TestMethod>]
    member this.DownloadThreePages () =
        (* По этому адресу точно нет ошибок *)
        let result = downloadAsync("http://htmlbook.ru/html/a/href")
        Assert.AreEqual(result.Length, 3)

        for res in result do
            match res with 
            | (_, Error(x)) -> Assert.Fail() 

            | _ -> Assert.IsTrue(true) (* Костыль! ♿*)

        let html = match result.Head with (_, Done(x)) -> x
        Assert.AreEqual(html.Length, 62477)

    [<TestMethod>]
    member this.WrongAdress () =
        let result = downloadAsync("http://d")
        Assert.AreEqual(result.Length, 1)

        let (_, isError) = result.Head
        match isError with 
        | Done(x) -> Assert.Fail() 

        | _ -> Assert.IsTrue(true) (* Костыль! ♿*)