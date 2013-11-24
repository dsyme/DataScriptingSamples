namespace global
open System

[<AutoOpen>]
module Async = 
    type Async<'T> with 
        /// Execute the asynchronous computation at most once
        // TODO: not currently thread safe
        static member Once p : Async<unit> = 
            let executed = ref false
            async { if not executed.Value then 
                        executed := true; 
                        do! p } 

        /// Gets an asynchronous computation that never returns, but is cancellable
        static member Never : Async<'T> = 
                async { let ctxt = System.Threading.SynchronizationContext.Current
                        let! token = Async.CancellationToken
                        let savedCCont = ref None
                        let registration : System.Threading.CancellationTokenRegistration = 
                            let onCancel _ = 
                                match savedCCont.Value with 
                                | None -> () 
                                | Some ccont -> 
                                    let cexn = OperationCanceledException("canceled")
                                    match ctxt with 
                                    | null -> System.Threading.ThreadPool.QueueUserWorkItem(fun _ -> ccont cexn)  |> ignore
                                    | ctxt -> ctxt.Post((fun _ -> ccont cexn), null)
                            token.Register(Action<obj>(onCancel), null)
                        return! Async.FromContinuations(fun (_,_,ccont) -> savedCCont := Some ccont) }

        static member ReturnToContext (f : unit -> Async<'T>) : Async<'T> = 
            async { let ctxt = System.Threading.SynchronizationContext.Current
                    match ctxt with 
                    | null -> return! f()
                    | c -> 
                        try 
                            let! v = f() 
                            do! Async.SwitchToContext c
                            return v 
                        with exn -> 
                            do! Async.SwitchToContext c
                            return! raise exn }

        static member ComputeOnNewThread (f : unit -> 'T) : Async<'T> = 
                  Async.ReturnToContext(fun () -> 
                      async { do! Async.SwitchToNewThread()
                              return f() })

#if SILVERLIGHT
module AsyncUtilities = 
    let SwitchToDispatcher() : Async<unit> = 
        if System.Windows.Deployment.Current.Dispatcher.CheckAccess() then 
           async.Return()
        else
           //let ctxt = System.Threading.SynchronizationContext.Current
           Async.FromContinuations(fun (scont,_,_) -> 
            do System.Windows.Deployment.Current.Dispatcher.BeginInvoke(System.Action< >(fun () -> scont())) |> ignore)

    let RunOnMainThread(f)= 
        async { do! SwitchToDispatcher()
                return f() }
         |> Async.RunSynchronously

#endif
