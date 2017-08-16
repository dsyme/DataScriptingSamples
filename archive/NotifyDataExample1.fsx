
//#load @"../../Scripts/FSharpChart.fsx" 

#load "extlib/NotifyData-0.1.fsx"
#load "packages/FSharp.Charting/FSharp.Charting.fsx"

open FSharp.Charting
open System
open System.Collections
open System.Collections.Generic
open System.Collections.Specialized
open System.Collections.ObjectModel
open System.Threading


Chart.Line [ for x in 0 .. 100 -> (x,x*x) ]

let sleep (n:int) = Thread.Sleep n

let animatedData1 = 
    seq { for i in 0 .. 10 do
              for x in 1..30 do 
                do sleep 50
                yield [ for i in 0 .. 100 -> (i,float x*float x*sin (float i)) ] }

let animatedData2 = 
    seq { for i in 0 .. 10 do
            for x in [1;2;3] do 
              do sleep 100
              yield (i,x*i) }

animatedData2 |> Seq.observe |> LiveChart.LineIncremental
animatedData1 |> Seq.observe |> LiveChart.Line


let form = new System.Windows.Forms.Form(Visible=true,TopMost=true)

form.MouseMove
   |> Event.map (fun e -> e.X, 500-e.Y) 
   |> LiveChart.LineIncremental


let rec circle t = 
    seq { yield (sin t, cos t) 
          do sleep 100
          yield! circle (t+0.1) } 


circle 0.0
   |> Seq.observe
   |> LiveChart.LineIncremental

let rnd = System.Random()

rnd.Next(100)

let rec circleWithWobbleInRadius (r,t) = 
    seq { yield (r*sin t, r*cos t) 
          do sleep 100
          yield! circleWithWobbleInRadius (r+rnd.NextDouble()-0.5,t+0.1) } 

circleWithWobbleInRadius (10.0,0.0)
   |> Seq.observe
   |> LiveChart.LineIncremental

let rec random (x,y) = 
    seq { yield (x,y) 
          do sleep 50
          yield! random (x+rnd.Next(11)-5, y+rnd.Next(11)-5)  } 

random (0,0)
   |> Seq.observe
   |> LiveChart.LineIncremental


let rec randomY (x,y) = 
    seq { yield (x,y) 
          do sleep 100
          let y2 = y+rnd.Next(11)-5 
          let y3 = 
              if y2 < -10 then y2 + 2 
              else if y2 > 10 then y2 - 2
              else y2
          yield! randomY (x+1, y3)  } 

randomY (0,0)
   |> Seq.observe
   |> LiveChart.LineIncremental


let rec circle2 t = 
    seq { yield (sin (t/2.0) + sin t, cos (t/4.0) + cos t) 
          do sleep 100
          yield! circle2 (t+0.1) } 
circle2 0.0
   |> Seq.observe
   |> LiveChart.LineIncremental

let rec circle3 t = 
    seq { yield (sin (t/5.0) + 3.0*sin t, cos (t/7.0) + 3.0*cos t) 
          do sleep 100
          yield! circle3 (t+0.1) } 
circle3 0.0
   |> Seq.observe
   |> LiveChart.PointIncremental

#r "packages/FSharp.Data/lib/net40/FSharp.Data.dll"

open FSharp.Data
let data = FreebaseData.GetDataContext()

seq { for x in data.Sports.Tennis.``Tennis Players`` do 
          yield Seq.length x.``Matches Won``, Seq.length x.``Matches Lost`` }
   |> Seq.observe
   |> LiveChart.PointIncremental

seq { for x in data.``Arts and Entertainment``.Film.Films do 
          if not (System.String.IsNullOrEmpty x.``Initial release date``) then 
             let ok,v = System.DateTime.TryParse x.``Initial release date``
             if ok then 
                yield v.Year }
   |> NotifySeq.ofSeq
   |> NotifySeq.countBy id
   |> Chart.Column 

(*
seq { for x in FreebaseData.``Science and Technology``.Chemistry.``Chemical Elements`` do 
          if not (System.String.IsNullOrEmpty x.``Discovery Date``) then 
             printfn "date = %A, elem = %s" x.``Discovery Date`` x.Name
             let ok,v = System.DateTime.TryParse x.``Discovery Date``
             if ok then 
                 yield (v.Year  / 100) * 100
             else 
                let ok,v = System.Int32.TryParse x.``Discovery Date``
                if ok then 
                    yield (v / 100) * 100
               }
   |> NotifySeq.ofSeq
   |> NotifySeq.countBy id
   |> UpdatingChart.Column 
*)
