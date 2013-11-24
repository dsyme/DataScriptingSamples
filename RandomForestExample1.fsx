open System
open System.Collections.Generic

#load "packages/FSharp.Charting/FSharp.Charting.fsx"
#load "minimath/RandomForest-0.1.fsx"
#load "credentials/WebDataCredentials.fsx"
open FSharp.Charting
open Samples.FSharp.Math.RandomForest

let summarize results = 
    for (v,tp,fp,fn,tn) in results do
        printfn "Label '%A'" v
        printfn "  True Positive: \t\t%i\nFalse Positive: \t%i\nFalse Negative: \t%i\nTrue Negative: \t\t%i" tp fp fn tn
        printfn "  Precision tp/(tp+fp): \t\t%f" (float tp / float (tp + fp))
        printfn "  Recall (tp/(tp+fn): \t%f" (float tp / float (tp + fn))
        printfn "  True negative (tn/(tn+fp): \t%f" (float tn / float (tn + fn))
        printfn "  Accuracy (tp + tn)/all: \t%f" (float (tp + tn) / float (tp + fp + tn + fn))
        printfn ""

let displaySummary results  =
        Chart.StackedColumn
            [ for (v,tp,fp,fn,tn) in results do 
                  yield  [ yield ("True Positive", float tp);
                           yield ("False Positive",float fp);
                           yield ("False Negative",float fn);
                           yield ("True Negative",float tn) ] ]

let analyze (actual:'T[]) (predicted:'T[]) =
    let count xs = xs |> Seq.lengthBy (fun x -> x)
    [ for v in Seq.distinct actual do 
        let tp = (predicted,actual) ||> Array.map2 (fun p a -> (p=v) && (a=v)) |> count 
        let fp = (predicted,actual) ||> Array.map2 (fun p a -> (p=v) && not(a=v)) |> count 
        let fn = (predicted,actual) ||> Array.map2 (fun p a -> not(p=v) && (a=v)) |> count 
        let tn = (predicted,actual) ||> Array.map2 (fun p a -> not(p=v) && not(a=v)) |> count 
        yield (v,tp,fp,fn,tn) ]



//-----------------------------------------------------------------------------
// Predict the periodic table block of a chemical element from its physical properties

#r @"packages/FSharp.Data/lib/net40/FSharp.Data.dll"
open FSharp.Data

type Freebase = FreebaseDataProvider<Key=API_KEY,NumIndividuals=2000>
let data = Freebase.GetDataContext()

let elements = data.``Science and Technology``.Chemistry.``Chemical Elements``

type item = Freebase.ServiceTypes.Chemistry.Chemistry.Chemical_elementData

// Poke around the data
[ for x in elements do 
   match x.``Periodic table block`` with 
   | null -> ()
   | b -> yield b.Name ]

let isValid (row: item) = 
    row.``Atomic mass`` <> null &&
    row.``Atomic mass``.Mass.HasValue &&
    row.``Boiling Point``.HasValue &&
    row.``Melting Point``.HasValue &&
    row.``Covalent radius``.HasValue &&
    row.``Ionization Energy``.HasValue &&
    row.``Van der Waals radius``.HasValue &&
    row.``Periodic table block`` <> null

let extractFeatures (row: item) = 
    [| row.``Atomic mass``.Mass.GetValueOrDefault() |> float
       row.``Boiling Point``.GetValueOrDefault() |> float
       row.``Melting Point``.GetValueOrDefault() |> float
       row.``Covalent radius``.GetValueOrDefault() |> float
       row.``Ionization Energy``.GetValueOrDefault() |> float
       row.``Van der Waals radius``.GetValueOrDefault()  |> float |]

let extractLabel (row: Freebase.ServiceTypes.Chemistry.Chemistry.Chemical_elementData) = 
    row .``Periodic table block``.Name 

let validData = elements  |> Seq.toArray |> Array.filter isValid

let rand = Random(0)
let trainingData, validation = rand.SplitData validData

let actualResults = validation |> Array.map extractLabel

let predictor = Predictor(trainingData, extractFeatures, extractLabel, rand=rand)

let decisionTreeResults =
    validation |> Array.map predictor.PredictUsingDecisionTree
               |> analyze actualResults

let randomForestResults = 
    validation |> Array.map predictor.PredictUsingRandomForest
               |> analyze actualResults 

printfn "Decision Tree"
decisionTreeResults  |> summarize

printfn "Random Forest, %d trees" predictor.TreeCount
randomForestResults        |> summarize    

// Display summary of results. Aim is to minimize false negatives
decisionTreeResults |> displaySummary
randomForestResults |> displaySummary

[ for x in trainingData do if (predictor.PredictUsingDecisionTree x <> extractLabel x) then yield x.Name ]
[ for x in trainingData do if (predictor.PredictUsingRandomForest x <> extractLabel x) then yield x.Name ]
[ for x in validation do if (predictor.PredictUsingDecisionTree x <> extractLabel x) then yield x.Name ]
[ for x in validation do if (predictor.PredictUsingRandomForest x <> extractLabel x) then yield x.Name ]

validation.[0].Name // Flourine
validation.[1].Name // Gold

let flourine = elements.Individuals.Fluorine
extractFeatures flourine
predictor.PredictUsingRandomForest flourine
predictor.PredictUsingDecisionTree flourine
flourine.``Periodic table block``.Name

let gold = elements.Individuals.Gold
extractFeatures gold
predictor.PredictUsingRandomForest gold
predictor.PredictUsingDecisionTree gold
gold.``Periodic table block``.Name

