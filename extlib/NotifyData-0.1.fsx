namespace global

open System
open System.Collections
open System.Collections.Generic
open System.Collections.Specialized
open System.Collections.ObjectModel


type INotifyEnumerable<'T> =
     inherit IEnumerable<'T>
     inherit INotifyCollectionChanged

module private Utils = 
    let ofObservableCollection (obs:ObservableCollection<'T>) = 
        { new obj() with 
              member x.ToString() = "INotifyEnumerable"
          interface INotifyEnumerable<'T> 
          interface IEnumerable<'T> with 
              member x.GetEnumerator() = (obs :> IEnumerable<'T>).GetEnumerator()
          interface IEnumerable with 
              member x.GetEnumerator() = (obs :> IEnumerable).GetEnumerator()
          interface INotifyCollectionChanged with 
              [<CLIEvent>]
              member x.CollectionChanged = (obs :> INotifyCollectionChanged).CollectionChanged }

    let replacing () = 
        let curr = ref [| |]
        let ev = Event<NotifyCollectionChangedEventHandler, NotifyCollectionChangedEventArgs>()
        let ctxt = System.Threading.SynchronizationContext.Current
        let coll = 
            { new obj() with 
                  member x.ToString() = "INotifyEnumerable from Seq.ofEvent"
              interface INotifyEnumerable<'T> 
              interface IEnumerable<'T> with 
                  member x.GetEnumerator() = (curr.Value :> IEnumerable<'T>).GetEnumerator()
              interface IEnumerable with 
                  member x.GetEnumerator() = (curr.Value :> IEnumerable).GetEnumerator()
              interface INotifyCollectionChanged with 
                  [<CLIEvent>]
                  member x.CollectionChanged = ev.Publish }
        let update elems = 
            let evArgs = NotifyCollectionChangedEventArgs(NotifyCollectionChangedAction.Reset)  // "dramatic change"
            curr := elems; ev.Trigger(curr,evArgs)
        coll, update

module Event = 
   let generateAsync f = 
       let ev = Event<_>()
       Async.StartImmediate (f (fun x -> ev.Trigger x))
       ev.Publish

module Seq = 

    [<AbstractClass>]
    type private BasicObserver<'T>() =
      let mutable stopped = false
      abstract Next : value : 'T -> unit
      abstract Error : error : exn -> unit
      abstract Completed : unit -> unit
      interface IObserver<'T> with
          member x.OnNext value = if not stopped then x.Next value
          member x.OnError e = if not stopped then stopped <- true; x.Error e
          member x.OnCompleted () = if not stopped then stopped <- true; x.Completed ()

    /// Run the sequence on a background thread and observe the results on the GUI thread
    let observe (xs : seq<_>) =
        { new IObservable<'U> with 
              member x.Subscribe(observer) =
                 let ctxt = System.Threading.SynchronizationContext.Current
                 let ct = new System.Threading.CancellationTokenSource()
                 async { try 
                            do! Async.SwitchToNewThread()
                            for i in xs do 
                                match ctxt with 
                                | null -> observer.OnNext i
                                | _ -> ctxt.Post ((fun _ -> observer.OnNext i),null) 
                            observer.OnCompleted()
                         with e -> 
                             match ctxt with
                             | null -> observer.OnError e
                             | _ -> ctxt.Post ((fun _ -> observer.OnError e),null)  }
                      |> fun p -> Async.Start (p,ct.Token)
                 { new System.IDisposable with member x.Dispose() = ct.Cancel() } } 


module NotifySeq = 

    let ofSeq (source:seq<'T>) : INotifyEnumerable<'T> = 
       let obs = new ObservableCollection<'T>()
       let ctxt = System.Threading.SynchronizationContext.Current
       async { do! Async.SwitchToNewThread()
               for i in source do 
                   match ctxt with 
                   | null -> obs.Add i
                   | _ -> ctxt.Post ((fun _ -> obs.Add i),null) } |> Async.Start
       Utils.ofObservableCollection obs

    let ofSeqReplacing (source:seq<#seq<'T>>) : INotifyEnumerable<'T> = 
       let coll, update = Utils.replacing ()
       let ctxt = System.Threading.SynchronizationContext.Current
       async { do! Async.SwitchToNewThread()
               for i in source do 
                  let elems = Seq.toArray i 
                  match ctxt with 
                  | null -> update elems
                  | _ -> ctxt.Post ((fun _ -> update elems),null) } 
          |> Async.Start
       coll

    // TODO: only start on connect + proper replay
    let ofObservable (source:IObservable<'T>) : INotifyEnumerable<'T> = 
        let obs = new ObservableCollection<'T>()
        source |> Observable.add (fun x -> obs.Add(x))
        Utils.ofObservableCollection obs

    // TODO: only start on connect + proper replay
    let ofObservableReplacing (source:IObservable<#seq<'T>>) : INotifyEnumerable<'T> = 
        let coll, update = Utils.replacing ()
        source |> Observable.add (fun elems -> update (Seq.toArray elems))
        coll

    let ofEvent (source:IEvent<_,_>)  = 
        let obs = new ObservableCollection<'T>()
        source |> Observable.add (fun x -> obs.Add(x))
        Utils.ofObservableCollection obs

    let ofEventReplacing (source:IEvent<_,_>) = 
        let coll, update = Utils.replacing ()
        source |> Observable.add (fun elems -> update (Seq.toArray elems))
        coll


    let private noNotify (obs : seq<_>) =
        { new obj() with 
              member x.ToString() = "INotifyEnumerable"
          interface INotifyEnumerable<'T> 
          interface IEnumerable<'T> with 
              member x.GetEnumerator() = (obs :> IEnumerable<_>).GetEnumerator()
          interface IEnumerable with 
              member x.GetEnumerator() = (obs :> IEnumerable).GetEnumerator()
          interface INotifyCollectionChanged with 
              member x.add_CollectionChanged(h) = ()
              member x.remove_CollectionChanged(h) = () }

    let zip (obs1 : INotifyEnumerable<'T>) (obs2 : INotifyEnumerable<'U>) =
        let obs = Seq.zip obs1 obs2 
        { new obj() with 
              member x.ToString() = "INotifyEnumerable"
          interface INotifyEnumerable<'T * 'U> 
          interface IEnumerable<'T * 'U> with 
              member x.GetEnumerator() = obs.GetEnumerator()
          interface IEnumerable with 
              member x.GetEnumerator() = (obs :> IEnumerable).GetEnumerator()
          interface INotifyCollectionChanged with 
              member x.add_CollectionChanged(h) = obs1.add_CollectionChanged(h); obs2.add_CollectionChanged(h) 
              member x.remove_CollectionChanged(h) = obs1.remove_CollectionChanged(h); obs2.remove_CollectionChanged(h) }

    let private adjust f (obs : INotifyEnumerable<'T>) =
        let newObs = f obs
        { new obj() with 
              member x.ToString() = "INotifyEnumerable"
          interface INotifyEnumerable<'U> 
          interface IEnumerable<'U> with 
              member x.GetEnumerator() = (newObs :> IEnumerable<_>).GetEnumerator()
          interface IEnumerable with 
              member x.GetEnumerator() = (newObs :> IEnumerable).GetEnumerator()
          interface INotifyCollectionChanged with 
              member x.add_CollectionChanged(h) = obs.add_CollectionChanged(h)
              member x.remove_CollectionChanged(h) = obs.remove_CollectionChanged(h) }

    let map f obs = adjust (Seq.map f) obs
    let countBy f obs = adjust (Seq.countBy f) obs
    let generateAsyncReplacing f = Event.generateAsync f |> ofEventReplacing
    let generateAsync f = Event.generateAsync f |> ofEvent

//module Seq = 
//    let observe (xs:seq<_>) = 

[<AutoOpen>]
module Extensions = 
    type IObservable<'Args> with 
        member x.ToNotifySeq() = NotifySeq.ofObservable x
        //member x.NotifyReplacing() = NotifySeq.ofObservableReplacing x

    type IEnumerable<'T> with 
        member x.ToNotifySeq() = NotifySeq.ofSeq x
(*
module Event = 
    let toObservableCollection (e:IEvent<_,_>) = 
        let oc = new System.Collections.ObjectModel.ObservableCollection<'T>()
        e.Add(fun x -> oc.Add x) 
        oc

module Seq = 
    let mutable ct = new System.Threading.CancellationTokenSource()
    let cancelPrevious() = ct.Cancel(); ct <- new System.Threading.CancellationTokenSource(); ct.Token
    let startAsUniqueEvent (s:seq<'T>) = 
        let ctxt = System.Threading.SynchronizationContext.Current
        if ctxt = null then invalidOp "This function may only be called from a thread where SynchronizationContext.Current is not null"
        let oc = new System.Collections.ObjectModel.ObservableCollection<'T>()
        let ev = new Event<_>()
        let job = async { for x in s do ctxt.Post((fun _ -> ev.Trigger x),null) } 
        Async.Start(job,cancelPrevious())
        ev.Publish
*)
