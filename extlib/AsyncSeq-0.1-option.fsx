namespace Samples.FSharp.AsyncSeq

//#load "AsyncEx-0.1.fsx"

open System

type IAsyncEnumerator<'T> =
    abstract MoveNext : unit -> Async<'T option>
    inherit IDisposable

type IAsyncEnumerable<'T> = 
    abstract GetEnumerator : unit -> IAsyncEnumerator<'T>

type AsyncSeq<'T> = IAsyncEnumerable<'T>

module private AsyncSeqImpl = 

    let private dispose (d:System.IDisposable) = match d with null -> () | _ -> d.Dispose()

    [<GeneralizableValue>]
    let empty<'T> : AsyncSeq<'T> = 
        { new IAsyncEnumerable<'T> with 
              member x.GetEnumerator() = 
                  { new IAsyncEnumerator<'T> with 
                        member x.MoveNext() = async { return None }
                        member x.Dispose() = () } }

    let singleton (v:'T) : AsyncSeq<'T> = 
        { new IAsyncEnumerable<'T> with 
              member x.GetEnumerator() = 
                  let state = ref 0
                  { new IAsyncEnumerator<'T> with 
                        member x.MoveNext() = async { let res = state.Value = 0
                                                      incr state; 
                                                      return (if res then Some v else None) }
                        member x.Dispose() = () } }

    let mapAsync (f: 'T -> Async<'U>) (inp: AsyncSeq<'T>) : AsyncSeq<'U> = 
        { new IAsyncEnumerable<'U> with 
              member x.GetEnumerator() = 
                  let state = ref -1
                  let enum = ref Unchecked.defaultof<_>
                  { new IAsyncEnumerator<'U> with 
                        member x.MoveNext() = 
                            async { match !state with 
                                    | -1 -> 
                                        enum := inp.GetEnumerator()
                                        state := 0
                                        return! x.MoveNext()
                                    | 0 ->   
                                        let! res = enum.Value.MoveNext() 
                                        match res with 
                                        | None -> 
                                            x.Dispose()
                                            return None
                                        | Some v ->
                                            let! v2 = f v
                                            return Some v2
                                    | _ -> 
                                        return None }
                        member x.Dispose() = 
                            match !state with 
                            | 0 -> 
                                let e = enum.Value
                                state := 1
                                enum := Unchecked.defaultof<_>
                                dispose e 
                            | _ -> () } }


    let make (f: unit -> Async<AsyncSeq<'T>>) : AsyncSeq<'T> = 
        { new IAsyncEnumerable<'T> with 
              member x.GetEnumerator() = 
                  let state = ref -1
                  let enum = ref Unchecked.defaultof<IAsyncEnumerator<'T>>
                  { new IAsyncEnumerator<'T> with 
                        member x.MoveNext() = 
                            async { match !state with 
                                    | -1 -> 
                                        let! s = f()
                                        let e = s.GetEnumerator()
                                        enum := e
                                        state := 0
                                        return! x.MoveNext()
                                    | 0 ->   
                                        let e = enum.Value
                                        let! res = e.MoveNext() 
                                        match res with
                                        | None -> 
                                            x.Dispose()
                                            return None
                                        | Some v ->
                                            return Some v
                                    | _ -> 
                                        return None }
                        member x.Dispose() = 
                            match !state with 
                            | 0 -> 
                                let e = enum.Value
                                state := 1
                                enum := Unchecked.defaultof<_>
                                dispose e 
                            | _ -> () } }

    let delay (f: unit -> AsyncSeq<'T>) : AsyncSeq<'T> = 
       make (fun () -> async { return f() })

    let bindAsync (f: 'T -> AsyncSeq<'U>) (inp : Async<'T>) : AsyncSeq<'U> = 
       make (fun () -> async { let! v = inp in return f v })

    let iteriAsync f (inp: AsyncSeq<_>)  = async { 
        use e = inp.GetEnumerator()
        let count = ref 0 
        let rec loop() = async {
              let! res = e.MoveNext()
              match res with 
              | None -> ()
              | Some v ->
                  do! f !count v 
                  incr count
                  return! loop()
        } 
        return! loop()
    }

    let append (inp1: AsyncSeq<'T>) (inp2: AsyncSeq<'T>) : AsyncSeq<'T> =
        { new IAsyncEnumerable<'T> with 
              member x.GetEnumerator() = 
                  let state = ref -1
                  let enum = ref Unchecked.defaultof<IAsyncEnumerator<'T>>
                  { new IAsyncEnumerator<'T> with 
                        member x.MoveNext() = 
                            async { match !state with 
                                    | -1 -> 
                                        enum := inp1.GetEnumerator()
                                        state := 0
                                        return! x.MoveNext()
                                    | 0 ->   
                                        let! res = enum.Value.MoveNext() 
                                        match res with 
                                        | None -> 
                                            x.Dispose()
                                            state := 1
                                            return! x.MoveNext()
                                        | Some _ -> 
                                            return res
                                    | 1 -> 
                                        enum := inp2.GetEnumerator()
                                        state := 2
                                        return! x.MoveNext()
                                    | 2 ->   
                                        let e = enum.Value
                                        let! res = e.MoveNext() 
                                        match res with
                                        | None -> 
                                            x.Dispose()
                                            state := 3
                                            return! x.MoveNext()
                                        | Some _ -> 
                                            return res
                                    | _ -> 
                                        return None }
                        member x.Dispose() = 
                            match !state with 
                            | 0 -> 
                                let e = enum.Value
                                state := 3
                                enum := Unchecked.defaultof<_>
                                dispose e 
                            | _ -> () } }


    // this pushes the handler through all the async computations
    let rec tryWith (inp: AsyncSeq<'T>) (handler : exn -> AsyncSeq<'T>) : AsyncSeq<'T> = 
        { new IAsyncEnumerable<'T> with 
              member x.GetEnumerator() = 
                  let state = ref -1
                  let enum = ref Unchecked.defaultof<IAsyncEnumerator<'T>>
                  // state -1: not started
                  // state 0: running
                  // state 1: finished
                  // state 2: exception happened, calling handler
                  // state 3: running handler
                  { new IAsyncEnumerator<'T> with 
                        member x.MoveNext() = 
                            async { match !state with 
                                    | -1 -> 
                                        let res = ref Unchecked.defaultof<_>
                                        try 
                                            res := Choice1Of2 (inp.GetEnumerator())
                                        with exn -> 
                                            res := Choice2Of2 exn
                                        match res.Value with
                                        | Choice1Of2 r ->
                                            enum := r
                                            state := 0
                                            return! x.MoveNext()
                                        | Choice2Of2 exn -> 
                                            x.Dispose()
                                            state := 2
                                            enum := (handler exn).GetEnumerator()
                                            state := 3
                                            return! x.MoveNext()
                                    | 0 ->   
                                        let e = enum.Value
                                        let res = ref Unchecked.defaultof<_>
                                        try 
                                            let! r = e.MoveNext()
                                            res := Choice1Of2 r
                                        with exn -> 
                                            res := Choice2Of2 exn
                                        match res.Value with 
                                        | Choice1Of2 (Some res) -> 
                                            return (Some res)
                                        | Choice1Of2 None -> 
                                            x.Dispose()
                                            return! x.MoveNext()
                                        | Choice2Of2 exn -> 
                                            x.Dispose()
                                            state := 2
                                            enum := (handler exn).GetEnumerator()
                                            state := 3
                                            return! x.MoveNext()
                                    | 3 ->   
                                        let e = enum.Value
                                        let! res = e.MoveNext() 
                                        match res with 
                                        | Some _ -> return res
                                        | None -> 
                                            x.Dispose()
                                            return! x.MoveNext()
                                    | _ -> 
                                        return None }
                        member x.Dispose() = 
                            match !state with 
                            | 0 | 3 -> 
                                let e = enum.Value
                                state := 1
                                enum := Unchecked.defaultof<_>
                                dispose e 
                            | _ -> () } }

    // this pushes the handler through all the async computations
    let rec tryFinally (inp: AsyncSeq<'T>) (compensation : unit -> unit) : AsyncSeq<'T> = 
        { new IAsyncEnumerable<'T> with 
              member x.GetEnumerator() = 
                  let state = ref -1
                  let enum = ref Unchecked.defaultof<IAsyncEnumerator<'T>>
                  { new IAsyncEnumerator<'T> with 
                        member x.MoveNext() = 
                            async { match !state with 
                                    | -1 -> 
                                        let e = inp.GetEnumerator()
                                        enum := e
                                        state := 0
                                        return! x.MoveNext()
                                    | 0 ->   
                                        let e = enum.Value
                                        let! res = e.MoveNext() 
                                        match res with 
                                        | None -> 
                                            x.Dispose()
                                            return! x.MoveNext()
                                        | Some _ -> 
                                            return res
                                    | _ -> 
                                        return None }
                        member x.Dispose() = 
                            match !state with 
                            | 0 -> 
                                let e = enum.Value
                                state := 1
                                enum := Unchecked.defaultof<_>
                                dispose e 
                                compensation()
                            | _ -> () } }

    let rec collectAsync (f: 'T -> AsyncSeq<'U>) (inp: AsyncSeq<'T>) : AsyncSeq<'U> = 
        { new IAsyncEnumerable<'U> with 
              member x.GetEnumerator() = 
                  let state = ref -1
                  let enum1 = ref Unchecked.defaultof<IAsyncEnumerator<'T>>
                  let enum2 = ref Unchecked.defaultof<IAsyncEnumerator<'U>>
                  { new IAsyncEnumerator<'U> with 
                        member x.MoveNext() = 
                            async { match !state with 
                                    | -1 -> 
                                        enum1 := inp.GetEnumerator()
                                        state := 0
                                        return! x.MoveNext()
                                    | 0 ->   
                                        let e1 = enum1.Value
                                        let! res1 = e1.MoveNext() 
                                        match res1 with
                                        | Some v1 ->
                                            enum2 := (f v1).GetEnumerator()
                                            state := 1
                                        | None -> 
                                            state := 2
                                        return! x.MoveNext()
                                    | 1 ->   
                                        let e2 = enum2.Value
                                        let! res2 = e2.MoveNext() 
                                        match res2 with 
                                        | None ->
                                            enum2 := Unchecked.defaultof<_>
                                            state := 0
                                            dispose e2
                                            return! x.MoveNext()
                                        | Some _ -> 
                                            return res2
                                    | 2 -> 
                                        return None
                                    | _ -> 
                                        return! invalidOp "enumerator already finished" }
                        member x.Dispose() = 
                            match !state with 
                            | 0 -> 
                                let e = enum1.Value
                                state := 3
                                enum1 := Unchecked.defaultof<_>
                                dispose e 
                            | 1 -> 
                                let e2 = enum1.Value
                                state := 0
                                dispose e2
                                x.Dispose()
                            | _ -> () } }

    let iterAsync (f: int -> 'T -> Async<unit>) (inp: AsyncSeq<'T>)  = iteriAsync (fun i x -> f i x) inp
    let iteri (f: int -> 'T -> unit) (inp: AsyncSeq<'T>)  = iteriAsync (fun i x -> async.Return (f i x)) inp
    let iter (f: 'T -> unit) (inp: AsyncSeq<'T>)  = iteriAsync (fun i x -> async.Return (f x)) inp

    /// Note: starts the asynchronous sequence in the thread pool
    /// Note: cancellation governed by default cancellation token
    let toBlockingSeq (x:AsyncSeq<_>) =
        seq { let queue = new System.Collections.Generic.Queue<_>()
              let wh = new System.Threading.AutoResetEvent(false)
              let completed = ref false
              let syncRoot = obj()
              let add v  = 
                  lock syncRoot (fun () -> queue.Enqueue v)
                  wh.Set() |> ignore
              let complete v = 
                  lock syncRoot (fun () -> completed := true)
                  wh.Set() |> ignore
              Async.Start (async { try 
                                       try 
                                         return! iter (Choice1Of2 >> add) x 
                                       with e -> 
                                         add (Choice2Of2 e)
                                   finally
                                       complete() })                            

              while lock syncRoot (fun () -> not !completed || queue.Count > 0) do 
                  while lock syncRoot (fun () -> queue.Count) > 0 do  
                      yield lock syncRoot (fun () -> queue.Dequeue())
                  if not !completed then 
                      wh.WaitOne()  |> ignore }
         |> Seq.map (function Choice1Of2 v -> v | Choice2Of2 err -> raise err)

    

type AsyncSeqBuilder() = 
    member __.Yield x = AsyncSeqImpl.singleton x
    member __.YieldFrom (x:AsyncSeq<'T>) = x

    // Allow values of type Async<'T> to be used with let! and do!
    member b.Bind (inp:Async<'T>, body : 'T -> AsyncSeq<'U>) : AsyncSeq<'U> = AsyncSeqImpl.bindAsync body inp

    // Allow values of type Async<'T> to be used with yield!
    member b.YieldFrom (x:Async<'T>) : AsyncSeq<'T> = b.Bind(x,(fun v -> b.Yield v))
                  
    member b.Zero () = AsyncSeqImpl.empty
              
    member b.Return(c:unit) =  b.Zero()
              
    member b.TryFinally (body: AsyncSeq<_>, compensation) = 
       AsyncSeqImpl.tryFinally body compensation
   
    member b.TryWith (body: AsyncSeq<_>, handler: (exn -> AsyncSeq<_>)) = 
       AsyncSeqImpl.tryWith body handler

    member b.Combine (seq1:AsyncSeq<'T>,seq2:AsyncSeq<'T>) = 
        AsyncSeqImpl.append seq1 seq2

    member b.While (gd, seq:AsyncSeq<'T>) = 
        if gd() then b.Combine(seq,b.Delay(fun () -> b.While (gd, seq))) else b.Zero()

    member b.Delay (seq:unit -> AsyncSeq<'T>) = 
        AsyncSeqImpl.delay seq

    // Allow "for" on asynchronous sequences
    member b.For (seq:AsyncSeq<'T>, action:'T -> AsyncSeq<'U>) = 
        AsyncSeqImpl.collectAsync action seq

    member b.Using (r : 'T :> IDisposable,f : 'T -> AsyncSeq<'U>) = 
        b.TryFinally(f r, r.Dispose)
 
    // Allow "for" on synchronous sequences
    member b.For (seq:seq<'T>, action:'T -> AsyncSeq<'U>) = 
        b.Using(seq.GetEnumerator(),(fun enum -> b.While(enum.MoveNext, b.Delay(fun () -> action enum.Current))))

[<AutoOpen>]
module AsyncSeqOperators = 

    let asyncSeq = AsyncSeqBuilder()


    module AsyncSeq = 

        [<GeneralizableValue>]
        let empty<'T> : AsyncSeq<'T> = AsyncSeqImpl.empty<'T> 
        let singleton v = AsyncSeqImpl.singleton v
        let mapAsync f inp = AsyncSeqImpl.mapAsync f inp
        let delay f = AsyncSeqImpl.delay f
        let bindAsync f inp = AsyncSeqImpl.bindAsync f inp
        let iteriAsync f inp = AsyncSeqImpl.iteriAsync f inp
        let iterAsync f inp = AsyncSeqImpl.iterAsync f inp
        let append inp1 inp2 = AsyncSeqImpl.append inp1 inp2
        let tryWith inp handler = AsyncSeqImpl.tryWith inp handler
        let tryFinally inp compensation =  AsyncSeqImpl.tryFinally inp compensation
        let collectAsync f inp = AsyncSeqImpl.collectAsync f inp
        let iteri p inp = AsyncSeqImpl.iteri p inp
        let iter p inp = AsyncSeqImpl.iter p inp

        // /// Note: starts the asynchronous sequence in the thread pool
        // /// Note: cancellation governed by default cancellation token
        // let toBlockingSeq (x:AsyncSeq<_>) = AsyncSeqImpl.toBlockingSeq x

        let toArrayAsync (source: AsyncSeq<'T>)  = 
            async { let res = ResizeArray()
                    do! source |> iter (fun v -> res.Add v)
                    return res.ToArray() }
                
        let toListAsync (source: AsyncSeq<'T>)  = 
            async { let! arr = toArrayAsync source
                    return Array.toList arr }

        let toList x = 
          x |> AsyncSeqImpl.toBlockingSeq |> Seq.toList

        let filterAsync (f: 'T -> Async<bool>) (source: AsyncSeq<'T>) : AsyncSeq<'T> = 
            asyncSeq { use ie = source.GetEnumerator() 
                       let! v = ie.MoveNext()
                       let b = ref v
                       while b.Value.IsSome do
                            let x = b.Value.Value
                            let! g = f x
                            if g then yield x
                            let! v = ie.MoveNext()
                            b := v }
        let chooseAsync (f: 'T -> Async<'U option>) (source: AsyncSeq<'T>) : AsyncSeq<'T> = 
            asyncSeq { use ie = source.GetEnumerator() 
                       let! v = ie.MoveNext()
                       let b = ref v
                       while b.Value.IsSome do
                            let x = b.Value.Value
                            let! g = f x
                            match g with
                            | None -> ()
                            | Some v -> yield x
                            let! v = ie.MoveNext()
                            b := v }

        let foldAsync (f: 'State -> 'T -> Async<'State>) (z:'State) (inp: AsyncSeq<'T>) : Async<'State> = 
            async { let zref = ref z
                    use ie = inp.GetEnumerator() 
                    let! v = ie.MoveNext()
                    let b = ref v
                    while b.Value.IsSome do
                        let! z = f zref.Value b.Value.Value
                        zref := z
                        let! v = ie.MoveNext()
                        b := v 
                    return zref.Value }
    
        let map (p: 'T -> 'U) (inp: AsyncSeq<'T>)  = AsyncSeqImpl.mapAsync (p >> async.Return) inp
        let filter (p: 'T -> bool) (inp: AsyncSeq<'T>)  = filterAsync (p >> async.Return) inp
        let choose (p: 'T -> 'U option) (inp: AsyncSeq<'T>)  = chooseAsync (p >> async.Return) inp
        let fold (f: 'State -> 'T -> 'State) (z:'State) (x: AsyncSeq<'T>) : Async<'State> = foldAsync (fun a b -> async.Return (f a b)) z x

        let ofObservable (ev: System.IObservable<'T>) : AsyncSeq<'T> = 
            asyncSeq { 
                let buffer = MailboxProcessor<_>.Start(fun inbox -> Async.Never)
                use _cleanup = ev |> Observable.subscribe buffer.Post 
                while true do 
                    yield! buffer.Receive() }

        let ofEvent (ev: IEvent<'T>) : AsyncSeq<'T> = ofObservable ev 

        let scanAsync<'T,'State> (f: 'State -> 'T -> Async<'State>) (z:'State) (source: AsyncSeq<'T>) : AsyncSeq<'State> = 
            asyncSeq { let zref = ref z
                       yield !zref
                       use ie = source.GetEnumerator() 
                       let! v = ie.MoveNext()
                       let b = ref v
                       while b.Value.IsSome do
                            let! z = f !zref b.Value.Value
                            zref := z
                            yield !zref 
                            let! v = ie.MoveNext()
                            b := v }

        let pairwise (source: AsyncSeq<'T>) =
            asyncSeq { let iref = ref None
                       for x in source do 
                            match !iref with 
                            | None -> ()
                            | Some prev -> yield (prev,x)
                            iref := Some x 
                      }

        let truncate (n:int) (source: AsyncSeq<'T>) =
            asyncSeq { let i = ref 0
                       use ie = source.GetEnumerator() 
                       let rec loop() = asyncSeq {
                           if !i < n then 
                               let! res = ie.MoveNext()
                               match res with 
                               | Some v ->
                                   yield v
                                   yield! loop()
                               | None -> ()
                       }
                       yield! loop() }
        /// Note: runs the sequence on a new thread 
        let ofSeq (inp:seq<'T>) =
            asyncSeq { 
                let ctxt = System.Threading.SynchronizationContext.Current
                let! ct = Async.CancellationToken
                let agent = 
                    MailboxProcessor<_>.Start(cancellationToken=ct, body=(fun inbox ->  
                        async { 
                            try 
                                do! Async.SwitchToNewThread()
                                use ie = inp.GetEnumerator()
                                while ie.MoveNext() do
                                    inbox.Post (Choice1Of2 ie.Current) 
                            with e -> 
                                    inbox.Post (Choice2Of2 e) 
                          
                        }))
                while true do 
                    // Note: agent.Receive does not do auto-return-to-synchronization context
                    let! msg = agent.Receive() 
                    match ctxt with 
                    | null -> ()
                    | c -> do! Async.SwitchToContext c
                    match msg with 
                    | Choice1Of2 v -> yield v
                    | Choice2Of2 err -> raise err
            }
                   
type AsyncSeq() =
    /// Note: runs the sequence on a new thread 
    static member StartAsEvent (source:AsyncSeq<'T>, ?cancellationToken) =
        let ev = new Event<_>()
        async { use ie = source.GetEnumerator() 
                let! v = ie.MoveNext()
                let b = ref v
                while b.Value.IsSome do
                    ev.Trigger b.Value.Value
                    let! v = ie.MoveNext() 
                    b := v }
            |> fun p -> Async.StartImmediate(p,?cancellationToken=cancellationToken)
        ev.Publish

module SeqAsyncDownloadHelpers = 

    open System.Collections.ObjectModel

    let private asyncDownloadOnceInto clear add (source: seq<'T>) = 
        let loadFinished = ref false
        let cachedSource = source |> Seq.cache
        async { try
                  if not loadFinished.Value then
                      do! cachedSource
                          |> AsyncSeq.ofSeq
                          |> AsyncSeq.iteri (fun i item -> 
                                if i = 0 then clear()
                                add item) 
                  loadFinished := true
                with e -> 
                  () }

    // Cached asynchronous sequence downloads. The downloads can be cancelled but the elements will remain in the cache and
    // re-iterate without further web requests.
    let cachedDownloadIntoObservableCollection (source: seq<'T>) = 
        let results = new ObservableCollection<'T>()
        let load = source |> asyncDownloadOnceInto results.Clear results.Add
        load, results

    let cachedDownloadIntoTwoLinkedObservableCollections (source: seq<'T * 'U>) = 
        let results1 = new ObservableCollection<'T>()
        let results2 = new ObservableCollection<'U>()
        let load = source |> asyncDownloadOnceInto (fun () -> results1.Clear(); results2.Clear()) (fun (a,b) -> results1.Add a; results2.Add b)
        load, results1, results2

#if TEST
AsyncSeq.empty |> AsyncSeq.iter (printfn "entry = %A") |> Async.RunSynchronously
let one = AsyncSeq.singleton "1"
one |> AsyncSeq.iter (printfn "entry = %A") |> Async.RunSynchronously
let two = AsyncSeq.append (AsyncSeq.singleton "1") (AsyncSeq.singleton "2")
two |> AsyncSeq.iter (printfn "entry = %A") |> Async.RunSynchronously
AsyncSeq.collectAsync (fun x -> AsyncSeq.singleton (x+"A")) one |> AsyncSeq.iter (printfn "entry = %A") |> Async.RunSynchronously
AsyncSeq.collectAsync (fun x -> AsyncSeq.singleton (x+"A")) two |> AsyncSeq.iter (printfn "entry = %A") |> Async.RunSynchronously
AsyncSeq.tryFinally two (fun () -> printfn "done!") |> AsyncSeq.iter (printfn "entry = %A") |> Async.RunSynchronously
AsyncSeq.tryWith two (fun e -> two) |> AsyncSeq.iter (printfn "entry = %A") |> Async.RunSynchronously
let fail :AsyncSeq<string> = AsyncSeq.delay (fun _ -> failwith "")
AsyncSeq.tryWith fail (fun e -> two) |> AsyncSeq.iter (printfn "entry = %A") |> Async.RunSynchronously
AsyncSeq.tryWith (AsyncSeq.append two fail) (fun e -> two) |> AsyncSeq.iter (printfn "entry = %A") |> Async.RunSynchronously

let test() = 
    let t = new System.Threading.CancellationTokenSource()
    let p = Async.Parallel [ Async.Never; async { do t.Cancel() } ] 
    let res = ref false
    try 
       Async.RunSynchronously(Async.TryCancelled(p, (fun _ -> res := true)),cancellationToken=t.Token) |> ignore
    with _ -> ()
    !res

test()


let obs1 = asyncSeq { yield 1; yield 2 }
let obs2 = asyncSeq { if 1 > 2 then yield 1 else yield 2 }

let check s a b = if a = b then printfn "succeeded: '%s'" s else printfn "failed: %s, expected %A, got %A" s b a

asyncSeq { yield 1 } |> AsyncSeq.iter (printfn "%A") |> Async.RunSynchronously 
check "ekew" (asyncSeq { yield 1 } |> AsyncSeq.toList) [1]

check "ekew" (asyncSeq { yield 1; 
                         yield 2 } |> AsyncSeq.toList) [1;2]

check "ekew" (try asyncSeq { failwith "I failed"; yield 1 } |> AsyncSeq.toList with Failure _ -> [2] ) [2]
check "ekew" (asyncSeq { try failwith "I failed"; yield 1 with Failure _ -> yield 2} |> AsyncSeq.toList) [2]
check "ekew" (let x = ref 0 in try asyncSeq { try failwith "I failed"; yield 1 finally x := 3 } |> AsyncSeq.toList with _ -> [!x]) [3]

check "ekew" (let x = ref 0 in (asyncSeq { try 
                                             yield 1 
                                           finally 
                                             x := 3 } |> AsyncSeq.toList) @ [!x]) [1; 3]
check "ekew" (let x = ref 0 in (asyncSeq { try 
                                             try 
                                                yield 1 
                                             finally x := 3 
                                           finally x := 4 } |> AsyncSeq.toList) @ [!x]) [1; 4]

check "ekew" (let x = ref 0 in (asyncSeq { try 
                                             yield 1 
                                           finally 
                                             x := 3 } |> AsyncSeq.toList) @ [!x]) [1; 3]

check "ekew" (asyncSeq { try 
                            yield 0; 
                            failwith "I failed"; 
                            yield 1 
                         with Failure _ -> yield 2} |> AsyncSeq.toList) [0;2]

check "ekew" (asyncSeq { if false then failwith "I failed" else yield 1 } |> AsyncSeq.toList) [1]

check "ekew" (asyncSeq { if 1 > 2 then yield 1 else yield 2 } |> AsyncSeq.toList) [2]

check "ekew" (asyncSeq { if 2 > 1 then yield 1 else yield 2 } |> AsyncSeq.toList) [1]

check "ekew" (asyncSeq { for x in asyncSeq { yield 1 } do yield (x,2) } |> AsyncSeq.toList) [(1,2)]

check "ekew" (asyncSeq { for x in asyncSeq { yield 1; yield 2 } do yield (x,'a') } |> AsyncSeq.toList) [(1,'a'); (2,'a')]
check "ekew" (asyncSeq { for x in asyncSeq { yield 1; 
                                             do! Async.Sleep 10
                                             yield 2 } do 
                            yield (x,'a') } |> AsyncSeq.toList) 
             [(1,'a'); (2,'a')]

for delay1 in [0;10;100] do
  for delay2 in [0;10;100] do
    check "ekew" (asyncSeq { for x in asyncSeq { yield 1; 
                                                 do! Async.Sleep delay1
                                                 yield 2 } do 
                                yield (x,'a') 
                                do! Async.Sleep delay2
                                yield (x,'b') } |> AsyncSeq.toList) 
                 [(1,'a'); (1, 'b'); (2,'a'); (2,'b') ]

check "ekew" (asyncSeq { for x in asyncSeq { yield 1; 
                                             do! Async.Sleep 100
                                             yield 2 } do 
                            yield (x,'a') 
                            do! Async.Sleep 10
                            yield (x,'b') } |> AsyncSeq.toList) 
             [(1,'a'); (1, 'b'); (2,'a'); (2,'b') ]


check "ekew" 
    (asyncSeq { for x in asyncSeq { yield 1 } do 
                  yield (x,'a') 
                  do! Async.Sleep 10
                  yield (x,'b') } |> AsyncSeq.toList) 
             [(1,'a'); (1, 'b') ]

check "ekew" 
    (asyncSeq { for x in asyncSeq { yield 1; 
                                    yield 2 } do 
                  yield (x,'a') 
                  do! Async.Sleep 100
                  yield (x,'b') } |> AsyncSeq.toList) 
             [(1,'a');  (1, 'b'); (2,'a'); (2,'b') ]

check "ekew" (asyncSeq { for x in asyncSeq { yield 1; yield 2 } do yield (x,'a') } |> AsyncSeq.toList) [(1, 'a'); (2, 'a')]

check "ekew" (asyncSeq { for x in asyncSeq { yield 1; yield 2 } do yield (x,'a'); yield (x,'b') } |> AsyncSeq.toList) [(1, 'a'); (1, 'b'); (2, 'a'); (2, 'b')]


check "ekew" (asyncSeq { for x in asyncSeq { yield 1; 
                                             do! Async.Sleep 100; 
                                             yield 2 
                                             do! Async.Sleep 100; } do 
                             do! Async.Sleep 10
                             yield (x,'a'); 
                             yield (x,'b') } |> AsyncSeq.toList) 
             [(1, 'a'); (1, 'b'); (2, 'a'); (2, 'b')]

check "ekew" 
    (AsyncSeq.toList 
            (asyncSeq { for x in asyncSeq { yield 1; 
                                            yield 2; } do 
                          printfn "A: x = %d" x; 
                          let! () = async { let! () = Async.Sleep (x*10) 
                                            printfn "here, x = %d" x
                                            return () } 
                          printfn "B: x = %d" x; 
                          yield x
                       
                        yield 3 }))
    [1;2;3]


check "ekew" 
    (AsyncSeq.toList (asyncSeq { for x in asyncSeq { yield 1; yield 2 } do 
                                   printfn "A: x = %d" x; 
                                   do! Async.Sleep (x*10) 
                                   printfn "B: x = %d" x; 
                                   yield x
                                 yield 3 }))
    [1;2;3]

check "ekew" 
    (asyncSeq { for x in asyncSeq { yield 1; 
                                    do! Async.Sleep 100  } do 
                  yield (x,'a') 
                  let! () = Async.Sleep 10
                  yield (x,'b') } |> AsyncSeq.toList) 
    [(1,'a'); (1, 'b') ]



check "ekew" 
    (asyncSeq {   yield (1,'a') 
                  do! Async.Sleep 100
                  yield (1,'b') } |> AsyncSeq.toList) 
    [(1,'a'); (1, 'b') ]


check "ekew" 
    (asyncSeq {   yield (1,'a') 
                  do! Async.Sleep 100} |> AsyncSeq.toList) 
             [(1,'a')]


check "ekew" (asyncSeq { for x in [1] do yield (x,2) } |> AsyncSeq.toList) [(1,2)]

check "ekew" (asyncSeq { try for x in [1] do 
                                  failwith "fail"; 
                                  yield 11 
                         with Failure _ -> 
                                  yield 1 } |> AsyncSeq.toList) [1]

check "ekew" (asyncSeq { try while true do 
                                  failwith "fail"; 
                                  yield 11 
                         with Failure _ -> 
                                  yield 1 } |> AsyncSeq.toList) [1]

check "ekew" (asyncSeq { let x = ref 0 
                         while !x > 0 do 
                             yield 11  } |> AsyncSeq.toList) []

check "ekew" (asyncSeq { let x = ref 3 
                         while !x > 0 do 
                             decr x
                             yield !x  } |> AsyncSeq.toList) [2;1;0]


check "ekew" (asyncSeq { printfn "sleeping!"
                         let! x = Async.Sleep 10
                         printfn "yielding!"
                         yield 100 } |> AsyncSeq.toList) [100]

check "ekew" (asyncSeq { printfn "sleeping!"
                         let! x = Async.Sleep 10
                         printfn "yielding!"
                         yield 100 
                         printfn "sleeping!"
                         let! x = Async.Sleep 10
                         printfn "yielding!"
                         yield 200 } |> AsyncSeq.toList) [100;200]

check "ekew" (asyncSeq { let x = ref 3 
                         while !x > 0 do 
                             printfn "sleeping!"
                             do! Async.Sleep 10
                             decr x
                             yield !x  } |> AsyncSeq.toList) [2;1;0]


#endif

(*
check "ekew" (Async.RunSynchronously (async { for x in asyncSeq { printfn "yielded"; yield 1 } do () })) ()
check "ekew" (Async.RunSynchronously (async { for x in asyncSeq { printfn "yielding"; yield 1; printfn "done";  } do () })) ()
*)
(*
check "ekew" (Async.RunSynchronously (async { let total = ref 0 
                                              for x in asyncSeq { yield 1; yield 2 } do 
                                                 printfn "x = %d" x; 
                                                 lock total (fun () -> total := !total + x)
                                                 do! Async.Sleep 10
                                              return !total })) 
             3
*)

(*
check "ekew" (Async.RunSynchronously (async { try 
                                                 for x in asyncSeq { do failwith "" } do 
                                                    printfn "x = %d" x; 
                                                    do! Async.Sleep 10 
                                                 return 1
                                              with Failure _ -> 
                                                  return 2 })) 2


check "ekew" (Async.RunSynchronously (async { let total = ref 0 
                                              for x in asyncSeq { yield 1; 
                                                                  do! Async.Sleep 100; 
                                                                  yield 2} do 
                                                  printfn "x = %d" x; 
                                                  do! Async.Sleep 200 
                                                  do! Async.Sleep 200 
                                                  printfn "adding %d" x; 
                                                  total := !total + x
                                              return !total }))
             3

check "ekew" (Async.RunSynchronously (async { let total = ref 0 
                                              for x in asyncSeq { yield 1; 
                                                                  do! Async.Sleep 100; 
                                                                  yield 2
                                                                  do! Async.Sleep 100; 
                                                                  () } do 
                                                  printfn "x = %d" x; 
                                                  // This sleep is short enough that we don't get cancelled
                                                  do! Async.Sleep 1 
                                                  // A second sleep gives a cancellation check, which in this case doesn't trigger
                                                  do! Async.Sleep 1 
                                                  printfn "adding %d" x; 
                                                  total := !total + x
                                              return !total }))
             3
*)


(*
public static AsyncSeq<IEnumerable<byte>> ReadAsyncWithLINQ(this Stream stream, int blockSize)
        {
            var asyncRead = Observable.FromAsyncPattern<byte[], int, int, int>(stream.BeginRead, stream.EndRead);
            var self = new BehaviorSubject<Unit>(new Unit());

            return from _ in self
                   let buffer = new byte[blockSize]
                   from bytesRead in asyncRead(buffer, 0, blockSize)
                   .Do(n => { if(n == 0) self.OnCompleted(); else self.OnNext(new Unit()); })
                   select buffer.Take(bytesRead);
        }

*)

//BeginRead/EndRead  --> AsyncSeq<byte[]>  of size N

(*
let readInChunks (stream: System.IO.Stream) chunkSize = 
    asyncSeq { let finished = ref false
               while not !finished do 
                   let bytes  = Array.zeroCreate chunkSize
                   let count = ref  0
                   while !count < chunkSize && not !finished do
                        let! numRead = Async.FromBeginEnd(bytes,!count,chunkSize - !count,stream.BeginRead, stream.EndRead)
                        finished := (numRead = 0)
                        count := !count + numRead
                   if !count > 0 then 
                       yield bytes.[0 .. !count-1] }


let long () = 
    asyncSeq { let i = ref 0 
               while !i < 10000 do 
                    if !i % 500 = 0 then 
                        //let! x = Async.Sleep 1
                        printfn "i = %d" !i 
                    incr i
                    yield !i }

check "cewe0" (long() |> AsyncSeq.toList) [ 1 .. 10000 ]


*)

(*
let readInChunks (stream: System.IO.Stream) chunkSize = 
    asyncSeq { while true do 
                   let bytes  = Array.zeroCreate chunkSize
                   let mutable count = 0
                   while count < chunkSize do
                        let! numRead = Async.FromBeginEnd(bytes,count,chunkSize - count,stream.BeginRead, stream.EndRead)
                        if (numRead = 0) then return
                        count <- count + numRead
                   yield bytes.[0 .. count-1] }
*)
             
          (*   
for start in 0 .. 1000 do
   for fin in start .. 1000 do 
       if (start + fin) % 100 = 0 then 
          printfn "start = %d, fin = %d" start fin
       let ms = new System.IO.MemoryStream( [| byte start .. byte fin |])
               
       readInChunks ms 10 |> AsyncSeq.toList |> Seq.toArray |> ignore

       *)

(*
let test () =
  let form = new System.Windows.Forms.Form() 
  form.Paint.Add(fun _ -> ());
  form
*)

(*
[<CompilationRepresentationAttribute(CompilationRepresentationFlags.UseNullAsTrueValue)>]
type Foo = 
   | Cons of int * Foo
   | Nil
let reduce gen = 
  match gen with 
  | Cons(_, Cons( _, Nil)) -> 
     printfn "path1"
     1
  | Cons (_, Nil) -> 
     printfn "path2"
     2
  | _ -> 
     3

printfn "result = %A" (reduce  (Cons (1, Cons (2, Nil))))

*)

(*
let reduce2<'T> (gen:int list) = 
  match gen with 
  | [_; _] -> 
     //printfn "path1, res = %d" (a + b)
     1 
  | [_] -> 
     //printfn "path2, a = %d" a
     0
  | _ -> 
     3


System.Console.WriteLine ("result = {0}", reduce2  [1;2])
*)
(*
let rec reduce3 (gen: int list) = 
    match gen with 
    | [_; _] -> 
       System.Console.WriteLine "path1"
       reduce3 gen.Tail
    | [_] -> 
       System.Console.WriteLine "path2"
       reduce3 gen.Tail
    | _ -> 
       System.Console.WriteLine "path3"
       
printfn "result = %A" (reduce3  [1;2])
*)

(*
let reduce4 gen = 
  let x =
      match gen with 
      | [_] -> 
         //printfn "path2, a = %d" a
         0
      | _ -> 
         3
  printfn "x = %A" x
  x
printfn "result = %A" (reduce4  [1;2])
*)

//let f x = match x with [_;_] -> printfn "path1"; 3  | [_] -> 2; | _ -> 3;;

//f [1;2];;



(*
type Agent<'T> = MailboxProcessor<'T>

type MailboxProcessor<'T> with 
     static member StartAndConsumeMessageStream(f) = 
          MailboxProcessor<'T>.Start(fun inbox ->  f (asyncSeq { while true do let! msg = inbox.Receive() in yield msg }))

let agent = Agent<int>.StartAndConsumeMessageStream(fun inStream -> inStream |> AsyncSeq.map string |> AsyncSeq.iter (printfn "%s"))          

agent.Post 3
agent.Post 4

let agents = [ for i in 0 .. 1000 -> 
                   Agent<int>.StartAndConsumeMessageStream(fun inStream -> 
                       inStream 
                       |> AsyncSeq.map string 
                       |> AsyncSeq.pairwise 
                       |> AsyncSeq.iter (printfn "%A"))  ]

for agent in agents do 
   agent.Post 3
for agent in agents do 
   agent.Post 4
for agent in agents do 
   agent.Post 5

   *)
