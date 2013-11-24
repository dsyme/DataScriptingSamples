namespace Samples.FSharp.Interop

#if BROWSER

#r "System.Windows"
#r "System.Windows.Browser"

open System
open System.Windows
open System.Windows.Browser


[<ScriptableType>]
type CallbackType(f: string * obj -> unit) = 

    [<ScriptableMember>]
    member x.Invoke(arg1:string, arg2:obj) = f(arg1,arg2)


[<AutoOpen>]
module private JsImpl = 
    open System.Reflection
    let uiasync (f: unit -> 'T) : Async<'T> = 
        Async.FromContinuations (fun (cont: 'T -> unit,econt,ccont) -> 
            let work () = try cont(f()) with e -> econt e
            let disp = System.Windows.Deployment.Current.Dispatcher
            let ok = disp.CheckAccess()
            if ok then work()
            else disp.BeginInvoke(new System.Action(work)) |> ignore)
    
    let uisync (f : unit -> 'T) : 'T = uiasync f |> Async.RunSynchronously       

    let jsasync (script:string) : Async<obj> = 
        async { try 
                   return! uiasync (fun () ->  HtmlPage.Window.Eval(script))
                with e ->
                   printfn "error evaluating JS script <<<\n%s\n>>>" script; return! raise e }

    let jsDoAsync s = jsasync s |> Async.Ignore 
    let jssync s = jsasync s |> Async.RunSynchronously       
    let jsvoid s = jssync s |> ignore
    let jsstart s = jsDoAsync s |> Async.Start

    uisync (fun () -> 
        let plugin = HtmlPage.Plugin
        let id = plugin.Id
        if id = "" then 
            plugin.SetProperty("id", "silverlight") |> ignore)

    let silverlightId = uisync (fun () -> HtmlPage.Plugin.Id)

    let mutable stamp = 0L
    let nextStamp() = stamp <- stamp + 1L; string stamp
    let nextCallback() = "SL" + nextStamp()

    let requests = new System.Collections.Generic.Dictionary<string,(obj -> unit)>()
    let invoke (tgt:string, obj:obj) = 
        let f = requests.[tgt] 
        requests.Remove tgt |> ignore; 
        f obj

    let xobj = CallbackType(invoke)
    
    uisync (fun () -> HtmlPage.RegisterScriptableObject("SL", xobj))

    let callback (f: 'T -> unit) = 
        let f2 (x:obj) = f (x :?> 'T)
        let stamp = nextStamp()
        requests.[stamp] <- f2
        "(function (arg) { return (document.getElementById('" + silverlightId + "')).Content.SL.Invoke('" + stamp + "',arg); })" 

    let callback0 (f: unit -> unit) = 
        let f2 (x:obj) = f ()
        let stamp = nextStamp()
        requests.[stamp] <- f2
        "(function () { document.getElementById('" + silverlightId + "').Content.SL.Invoke('" + stamp + "',''); })" 


module Js = 
    let doAsync script = jsasync script |> Async.Ignore 
    let startAsync s = jsDoAsync s |> Async.Start
    let callback f = JsImpl.callback f
    let callback0 f = JsImpl.callback0 f
    let silverlightId = JsImpl.silverlightId

module Json =     
    let convertible (x: IConvertible) = 
        match x with 
        | :? string as s -> "'" + s + "'"
        | _ -> System.Convert.ToString x
    let seq f xs = "[" + String.concat ", " (Seq.map f xs) + "]"

#endif
