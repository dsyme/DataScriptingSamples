module Samples.FSharp.Math.KMeans

#load "../extlib/ArrayEx-0.1.fsx"
#load "../extlib/SeqEx-0.1.fsx"


type Input<'T> = 
   { Data : 'T 
     Features: float[] }

type Centroid = float[]

/// Compute the norm distance between an input and a centroid
let distance (xs:Input<_>) (ys:Centroid) =
    (xs.Features,ys) 
        ||> Array.map2 (fun x y -> (x - y) * (x - y))
        |> Array.sum

/// Find the average of set of inputs. First compute xs1 + ... + xsN, pointwise, 
/// then divide each element of the sum by the number of inputs.
let computeCentroidOfGroup (_,group:Input<_>[]) =
    let e0 = group.[0].Features
    [| for i in 0 .. e0.Length - 1 -> group |> Array.averageBy (fun e -> e.Features.[i]) |]

/// Group all the inputs by the nearest centroid
let classifyIntoGroups inputs centroids = 
    inputs |> Array.classifyBy (fun v -> centroids |> Array.minBy (distance v))

/// Repeatedly classify the inputs, starting with the initial centroids
let rec computeCentroids inputs centroids = 
    seq { let classification = classifyIntoGroups inputs centroids
          yield classification
          let newCentroids = Array.map computeCentroidOfGroup classification
          yield! computeCentroids inputs newCentroids }

let kmeans inputs featureExtractor initialCentroids = 
    let inputs = inputs |> Seq.map (fun i -> { Data = i; Features = featureExtractor i }) |> Seq.toArray
    let initialCentroids = initialCentroids |> Seq.toArray
    computeCentroids inputs initialCentroids



(*
module Samples.KMeans.Units


type Input<'T, [<Measure>] 'u> = 
   { Data : 'T 
     Features: float<'u>[] }

type Centroid<[<Measure>] 'u> = float<'u>[]


/// Compute the norm distance between an input and a centroid
let distance (xs:Input<_,_>) (ys:Centroid<_>) =
    (xs.Features,ys) 
        ||> Array.map2 (fun x y -> (x - y) * (x - y))
        |> Array.sum

/// Find the average of set of inputs. First compute xs1 + ... + xsN, pointwise, 
/// then divide each element of the sum by the number of inputs.
let computeCentroidOfGroup (group:Input<_,_>[]) =
    let e0 = group.[0].Features
    [| for i in 0 .. e0.Length - 1 -> group |> Array.averageBy (fun e -> e.Features.[i]) |]

/// Group all the inputs by the nearest centroid
let classifyIntoGroups inputs centroids = 
    inputs |> Array.classifyBy (fun v -> centroids |> Array.minBy (distance v))

/// Repeatedly classify the inputs, starting with the seed centers
let computeCentroids inputs initialCentroids = 
    initialCentroids |> Seq.iterate (classifyIntoGroups inputs >> Array.map computeCentroidOfGroup) 

let kmeans inputs initialCentroids = 
    computeCentroids inputs initialCentroids |> Seq.map (fun centroids -> centroids, classifyIntoGroups inputs centroids)

*)

