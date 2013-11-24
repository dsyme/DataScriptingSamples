
//#r "../TypeProviders/Debug/net40/Samples.Hadoop.TypeProviders.dll"
#load "minimath/RandomForest-0.1.fsx"
#load "packages/FSharp.Charting/FSharp.Charting.fsx"
#r "packages/FSharp.Data/lib/net40/FSharp.Data.dll"

open FSharp.Charting
open System
open System.Collections.Generic
open Samples.FSharp.Math.RandomForest
//open Samples.Hadoop
//type Hive = HiveTypeProvider<"127.0.0.1",UseUnitAnnotations = false, UseRequiredAnnotations = true>

[<Literal>] 
let file = __SOURCE_DIRECTORY__ + "/data/WhiteWineExampleData.csv"
type WhiteWineData = FSharp.Data.CsvProvider<file>

let (X,y) = 
    let rows = WhiteWineData.Load(file) .Data |> Seq.toArray
    let X = rows |> Array.map (fun row -> [|row.``fixed acidity``; row.``volatile acidity``; row.``citric acid``; row.``residual sugar``; row.chlorides; row.``free sulfur dioxide``;
                                            row.``total sulfur dioxide``; row.density; row.pH; row.sulphates; row.alcohol|])
    let y = rows |> Array.map (fun row -> row.quality)
    let X = X |> Array.map (Array.map float)
    (X,y)
    

let rand = Random(0)
let featureCount = 11
let featureSampleCount = 6 // typcially ~ sqrt featureCount
let features = [|0..featureCount - 1|]
let train,validation = [|0..X.Length-1|] |> Array.partition (fun _ -> rand.Next(3) <> 0)
let actualResults = validation |> Array.map (fun v -> y.[v])
let treeCount = 100
let Xtrain = [| for i in train -> X.[i] |] 


// TODO - remove the weights
let weights = [| for i in train -> 1.0 |] 

// build the Random Forest    
let forest = buildRandomForest(Xtrain,y,weights,features,treeCount,featureSampleCount,rand)

// Build the confusion sparse matrix
let confusion = 
    let confusion = new Dictionary<int*int,int>()
    for v in validation do
        let (actual,predicted,_) =
            forest |> Array.map (fun tree -> (runDecisionTree X.[v] tree, y.[v]))
                    |> Seq.groupBy (fst) 
                    |> Seq.map (fun (x,xs) -> (snd (xs |> Seq.head), x, xs |> Seq.length))
                    |> Seq.maxBy (fun (x,y,count) -> count)
        match confusion.TryGetValue((actual,predicted)) with
        | (true,value) -> confusion.[(actual,predicted)] <- value + 1
        | _ -> confusion.Add((actual,predicted), 1) |> ignore
    confusion

let correct = confusion |> Seq.map (fun kv -> if fst kv.Key = snd kv.Key then kv.Value else 0) |> Seq.sum 

do  //Print Results
    printfn "Accuracy: %f\n" (float correct / float validation.Length)

    printfn "Confusion Matrix:"
        
    for kv1 in weights do printf "\t%f" kv1
    printfn ""
    for kv1 in weights do 
        printf "%i" kv1.Key
        for kv2 in weights do
            let confusion = match confusion.TryGetValue((kv1.Key,kv2.Key)) with
                            | (true,value) -> value 
                            | _ -> 0
            printf "\t%i" confusion
        printfn ""
    printfn ""

// TODO - Improve top feature selection process
printfn "Top Features"
let topFeatures =
    forest |> Array.map (fun (DecisionChoice(d)) -> d.Indicator) 
            |> Seq.groupBy (fun x -> x)
            |> Seq.map (fun (x,xs) -> (x,xs |> Seq.length))
            |> Seq.sortBy snd
            |> Seq.toArray
            |> Array.rev

topFeatures |> Array.iteri (fun i (x,_) -> printfn "%i:\t%s" (i+1) headers.[x])


// Chart the Confusion Matrix
(Chart.Combine [
                Chart.Line([(0.,0.);(10.,10.)])
                Chart.Bubble(confusion |> Seq.filter (fun x -> (fst x.Key <> snd x.Key)) |>  Seq.map (fun x -> (float(fst x.Key), (float(snd x.Key), float x.Value / 100.)))).AndMarkers(Style = ChartStyles.MarkerStyle.Circle, Color = Drawing.Color.LightBlue);
                Chart.Bubble(confusion |> Seq.filter (fun x -> (fst x.Key = snd x.Key)) |> Seq.map (fun x -> (float(fst x.Key), (float(snd x.Key), float x.Value / 100.)))).AndMarkers(Style = ChartStyles.MarkerStyle.Circle, Color = Drawing.Color.LightGreen);
                ])
    .WithXAxis(Title = "Actual")
    .WithYAxis(Title = "Predicted")

// Chart Dominant Features
Chart.Bar(topFeatures |> Seq.map (fun (h,c) -> (headers.[h],c)) |> Seq.sortBy snd)
    .WithTitle("Dominant Features")
    .WithXAxis(Title = "Top Usage")
    .WithYAxis(Title = "Feature Names")
