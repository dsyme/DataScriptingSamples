namespace global

#load "jslib--JsCalls-0.1.fsx"
//#load "extlib/AsyncEx-0.1.fsx"
#if BROWSER
module AsyncUtilities = 
    let SwitchToDispatcher() : Async<unit> = 
        if System.Windows.Deployment.Current.Dispatcher.CheckAccess() then 
           async.Return()
        else
           //let ctxt = System.Threading.SynchronizationContext.Current
           Async.FromContinuations(fun (scont,_,_) -> 
            do System.Windows.Deployment.Current.Dispatcher.BeginInvoke(System.Action< >(fun () -> scont())) |> ignore)


[<System.Windows.Browser.ScriptableType>]
type JsonpRequest private () as this =
    static let theObj = JsonpRequest()
    let mutable stamp = 0L
    let nextStamp() = stamp <- stamp + 1L; string stamp
    let requests = new System.Collections.Generic.Dictionary<string,(string -> unit)>()
    do 
       System.Windows.Browser.HtmlPage.RegisterScriptableObject("SL",this)
       if System.Windows.Browser.HtmlPage.Plugin.Id = "" then 
           System.Windows.Browser.HtmlPage.Plugin.SetProperty("id", "silverlight")
    let id = System.Windows.Browser.HtmlPage.Plugin.Id

    static let jQueryVersion = "1.4"
    static let loadJQuery =
        Async.Once <| Async.FromContinuations(fun (cont,econt,_) -> 
              try 
                Js.startAsync ("""
                      // Dynamically injects 
                      //     <script type="text/javascript" src="http://ajax.googleapis.com/ajax/libs/jquery/1.4/jquery.min.js"></script>
                      var script2 = document.createElement('script');
                      script2.setAttribute('src', 'http://ajax.googleapis.com/ajax/libs/jquery/""" + version + """/jquery.min.js');
                      script2.onload = """ + Js.callback0 cont + ";")
              with e -> econt e)

    static member GetAsync(url:string) : Async<string> = theObj.GetAsyncImpl(url)

    static member GetSync(url:string) : string = 

        if System.Windows.Deployment.Current.Dispatcher.CheckAccess() then 
            invalidOp "must use GetAsync when calling from user interface thread"

        let resultTask = System.Threading.Tasks.TaskCompletionSource<string>()
        async { 
           do! AsyncUtilities.SwitchToDispatcher()
           do! loadJQuery
           let! jsonText = JsonpRequest.GetAsync(url)
           resultTask.SetResult jsonText
        } |> Async.StartImmediate
        resultTask.Task.Result

    // TODO: add textStatus, jqXHR arguments to callback
    member private x.GetAsyncImpl(url:string) : Async<string> = 
          Async.FromContinuations<string>(fun (scont,_econt,_ccont) ->
              Js.startAsync(sprintf "jQuery.getJSON('%s',  function(data) { %s(JSON.stringify(data)); });" url (Js.callback scont)) |> ignore)

#endif

//e.g.
//
        //JsonpRequest.LoadJQuery("1.4")
        //async { do! Async.Sleep 1000
        //        let jsText1 = JsonpRequest.GetSync("http://api.flickr.com/services/feeds/photos_public.gne?tags=cat&tagmode=any&format=json&jsoncallback=?") 
        //        let jsText2 = JsonpRequest.GetSync("http://api.flickr.com/services/feeds/photos_public.gne?tags=dog&tagmode=any&format=json&jsoncallback=?") 
        //        return() }
        //      |> Async.Start
