#load "packages/FSharp.Charting/FSharp.Charting.fsx"
#load "extlib/ArrayEx-0.1.fsx"
#load "minimath/KMeans-0.1.fsx"

#r "bin/Samples.Hadoop.TypeProviders.dll"
#r "bin/HadoopHiveProxyLib.dll"

open FSharp.Charting
open Samples.FSharp.Math
open Samples.Hadoop

type T1 = HiveTypeProvider<"tryfsharp">

let ctxt = T1.GetDataContext()

let data = query { for i in ctxt.iris do select i } |> Seq.toArray

let extractFeatures (i:T1.DataTypes.iris) = [|i.petalLength; i.petalWidth; i.sepalLength; i.sepalWidth|]

let initialCentroids = (data |> Seq.take 3 |> Seq.toArray |> Array.map extractFeatures)


let layouts =  KMeans.kmeans data  (fun i -> [|i.petalLength; i.petalWidth; i.sepalLength; i.sepalWidth|]) initialCentroids
    
layouts |> Seq.map (fun groups -> 
      Chart.Combine (groups |> Seq.mapi (fun k (centroid,inps) -> 
        let color = 
            match k with
                | 1 -> System.Drawing.Color.Red
                | 2 -> System.Drawing.Color.Green
                | _ -> System.Drawing.Color.Blue
        (Chart.Point(centroid |> Seq.map (fun x -> x.[i],x.[j]), Color = color)))))
    |> Chart.Rows

