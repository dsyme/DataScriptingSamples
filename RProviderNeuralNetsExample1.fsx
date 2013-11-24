#load @"packages\Deedle\Deedle.fsx"
#load @"packages\RProvider\RProvider.fsx"

open RProvider.utils
//R.install_packages("neuralnet")
//R.install_packages("caret")


open Deedle
open RDotNet
open RProvider
open RProvider.``base``
open RProvider.datasets
open RProvider.neuralnet
open RProvider.caret
 
let iris : Frame<int, string> = R.iris.GetValue()

let features =
    iris
    |> Frame.filterCols (fun c _ -> c <> "Species")
    |> Frame.mapColValues (fun c -> c.As<double>())

let targets =
    R.as_factor(iris.Columns.["Species"].As<int>())
 
R.featurePlot(x = features, y = targets, plot = "pairs")

let range = [1..iris.RowCount]
let trainingIdxs : int[] = R.sample(range, iris.RowCount*7/10).GetValue()
let testingIdxs  : int[] = R.setdiff(range, trainingIdxs).GetValue()
let trainingSet = iris.Rows.[trainingIdxs]
let testingSet  = iris.Rows.[testingIdxs]

let nn =
  R.neuralnet(
    "Species ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width",
    data = trainingSet, hidden = R.c(3,2),
    err_fct = "ce", linear_output = true)
 
// Plot the resulting neural network with coefficients
R.plot_nn nn

let testingFeatures =
    testingSet
    |> Frame.filterCols (fun c _ -> c <> "Species")
    |> Frame.mapColValues (fun c -> c.As<double>())
let testingTargets =
    testingSet.Columns.["Species"].As<int>().Values

let prediction =
    R.compute(nn, testingFeatures)
     .AsList().["net.result"].AsVector()
    |> Seq.cast<double>
    |> Seq.map (round >> int)

let misclassified =
    Seq.zip prediction testingTargets
    |> Seq.filter (fun (a,b) -> a<>b)
    |> Seq.length

 
 
printfn "Misclassified irises '%d' of '%d'" misclassified (testingSet.RowCount)
