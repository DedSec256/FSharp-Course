namespace Tests

open System
open Microsoft.VisualStudio.TestTools.UnitTesting
open Implementations
open Interface
open LazyFactory
open System.Threading
open System.Threading.Tasks

[<TestClass>]
type TestClass () =

    [<TestMethod>]
    member this.``SingleThreadLazy Test`` () =
        let singleLazy = LazyFactory.CreateSingleThreadedLazy<int>(fun () -> 1)

        Assert.IsFalse(singleLazy.isValueCreated)
        Assert.AreEqual((singleLazy :> ILazy<int>).Get(), 1)
        Assert.IsTrue(singleLazy.isValueCreated)

    [<TestMethod>]
    member this.``SingleThreadLazy Test For Null`` () =
        let singleLazy = LazyFactory.CreateSingleThreadedLazy<obj>(fun () -> null)

        Assert.IsFalse(singleLazy.isValueCreated)
        Assert.IsNull((singleLazy :> ILazy<obj>).Get())
        Assert.IsTrue(singleLazy.isValueCreated)

    [<TestMethod>]
    member this.``MultiThreadLazy Test For Single Thread`` () =
        let multiLazy = LazyFactory.CreateMultiThreadedLazy<int>(fun () -> 1)

        Assert.IsFalse(multiLazy.isValueCreated)
        Assert.AreEqual((multiLazy :> ILazy<int>).Get(), 1)
        Assert.IsTrue(multiLazy.isValueCreated)

    (* Запускайте этот тест отдельно от остальных, ибо у меня, например, VS крашится, если не отдельно.
       Однозначно проблемы в Thread *)
    [<TestMethod>]
    member this.``MultiThreadLazy Test For MULTI Threads`` () =

        let multiLazy = LazyFactory.CreateMultiThreadedLazy<int>(fun () -> 1)
        Parallel.For(1, 10, fun i -> Assert.AreEqual(multiLazy.Get(), 1)) |> ignore
        Thread.Sleep(3000) (* Знаю, что убого. Но проще всего *)

    [<TestMethod>]
    member this.``LockFreeLazy Test For Single Thread`` () =
        let freeLazy = LazyFactory.CreateLockFreeLazy<obj>(fun () -> box 1)

        Assert.IsFalse(freeLazy.isValueCreated)
        Assert.AreEqual((freeLazy :> ILazy<obj>).Get() |> unbox, 1)
        Assert.IsTrue(freeLazy.isValueCreated)

    [<TestMethod>]
    member this.``LockFreeLazy Test For Null`` () =
        let freeLazy = LazyFactory.CreateLockFreeLazy<obj>(fun () -> null)

        Assert.IsFalse(freeLazy.isValueCreated)
        Assert.IsNull((freeLazy :> ILazy<obj>).Get())
        Assert.IsTrue(freeLazy.isValueCreated)
        
    (* Запускайте этот тест отдельно от остальных, ибо у меня, например, VS крашится, если не отдельно *)
    [<TestMethod>]
    member this.``LockFreeLazy Test For MULTI Threads`` () =

        let lockLazy = LazyFactory.CreateLockFreeLazy<obj>(fun () -> box 1)
        Parallel.For(1, 10, fun i -> Assert.AreEqual(lockLazy.Get() |> unbox, 1)) |> ignore
        Thread.Sleep(3000) (* Знаю, что убого. Но проще всего *)