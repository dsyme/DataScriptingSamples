#load "packages/FSharp.Charting/FSharp.Charting.fsx"
#load "minimath/KMeans-0.1.fsx"

open FSharp.Charting
open FSharp.Charting.ChartTypes
open Samples.FSharp.Math.KMeans
open Microsoft.FSharp.Data.UnitSystems.SI.UnitSymbols

type Observation = { Time: float<s>; Location: float<m> }

let rnd = System.Random()
let rand() = rnd.NextDouble() 
let randZ() = rnd.NextDouble() - 0.5

let near p = { Time= p.Time + randZ() * 70.0<s>; Location = p.Location + randZ() * 70.0<m> }

let data = 
     [ for i in 1 .. 1000 -> near { Time= 100.0<s>; Location = 60.0<m> }
       for i in 1 .. 1000 -> near { Time= 120.0<s>; Location = 80.0<m> }
       for i in 1 .. 1000 -> near { Time= 180.0<s>; Location = 30.0<m> }
       for i in 1 .. 1000 -> near { Time= 70.0<s>; Location = 40.0<m> } ]

let maxTime = data |> Seq.maxBy (fun p -> p.Time) |> fun p -> p.Time
let maxLoc = data |> Seq.maxBy (fun p -> p.Location) |> fun p -> p.Location

let initialCentroids = [ for i in 0 .. 9 -> [| rand(); rand() |] ]
let featureExtractor (p:Observation) = [| p.Time / maxTime; p.Location / maxLoc |]

let firstCentroids = 
    kmeans data featureExtractor initialCentroids
       |> Seq.map (Array.map (fun (c,_) -> c.[0] * maxTime, c.[1] * maxLoc))
       |> Seq.nth 1

let finalCentroids = 
    kmeans data featureExtractor initialCentroids
       |> Seq.map (Array.map (fun (c,_) -> c.[0] * maxTime, c.[1] * maxLoc))
       |> Seq.nth 100 

Chart.Combine 
  [ Chart.Point [ for i in data -> i.Time, i.Location ]

    Chart.Point finalCentroids ]

Chart.Combine 
  [ Chart.Point [ for i in data -> i.Time, i.Location ]

    Chart.Point firstCentroids ]
