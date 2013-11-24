open System
open System.Collections.Generic
//#r "../TypeProviders/Debug/net40/Samples.Hadoop.TypeProviders.dll"

#load "packages/FSharp.Charting.0.82/FSharp.Charting.fsx"
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

#r @"../TypeProviders/Debug/net40/Samples.DataStore.Freebase.dll"
open Samples.DataStore.Freebase

type Freebase = FreebaseDataProvider<Key=API_KEY,NumIndividuals=2000>
let data = Freebase.GetDataContext()

let elements = data.``Science and Technology``.Chemistry.``Chemical Elements``

[ for x in elements do 
   match x.``Periodic table block`` with 
   | null -> ()
   | b -> yield b.Name ]

let isValid (row: Freebase.ServiceTypes.Chemistry.Chemistry.Chemical_elementData) = 
    row.``Atomic mass`` <> null &&
    row.``Atomic mass``.Mass.HasValue &&
    row.``Boiling Point``.HasValue &&
    row.``Melting Point``.HasValue &&
    row.``Covalent radius``.HasValue &&
    row.``Ionization Energy``.HasValue &&
    row.``Van der Waals radius``.HasValue &&
    row.``Periodic table block`` <> null

let extractFeatures (row: Freebase.ServiceTypes.Chemistry.Chemistry.Chemical_elementData) = 
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

let predictor = Predictor(trainingData, extractFeatures, extractLabel)

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
[ for x in validation do if (predictor.PredictUsingDecisionTree x <> extractLabel x) then yield x.Name ]
[ for x in validation do if (predictor.PredictUsingRandomForest x <> extractLabel x) then yield x.Name ]

validation.[0].Name // copper
validation.[1].Name // Iodine

let copper = elements.Individuals.Copper
extractFeatures copper
predictor.PredictUsingRandomForest copper
predictor.PredictUsingDecisionTree copper
copper.``Periodic table block``.Name

let iodine = elements.Individuals.Iodine
extractFeatures iodine
predictor.PredictUsingRandomForest iodine
predictor.PredictUsingDecisionTree iodine
iodine.``Periodic table block``.Name




//-----------------------------------------------------------------------------
// Predict the periodic table block of a chemical element from its physical properties
//
// Explored: books, stars, cyclones, films

(*
let rows = 
    query { for x in data.``Arts and Entertainment``.Comics.``Comic Book Colorists`` do 
            take 100
            select x }
    |> Seq.toArray


// TODO: "bad request" when 'take 100'
let rows = 
    query { for x in data.``Arts and Entertainment``.Film.``Film actors`` do 
            take 30
            select x }
    |> Seq.toArray

// TODO: "bad request" when 'take 100'
let rows = 
    query { for x in data.``Arts and Entertainment``.Film.Films  do 
            take 40
            select x }
    |> Seq.toArray

let rows = 
    query { for x in data.``Science and Technology``.Meteorology.``Tropical Cyclones`` do 
            take 200
            select x }
    |> Seq.toArray

type input = Freebase.ServiceTypes.Meteorology.Meteorology.Tropical_cycloneData

let isValid (row: input) = 
    //row.Age <> null &&
    //row.Age.Age.HasValue &&
    //row.``Absolute Magnitude``.HasValue &&
    //row.``Color Index``.HasValue &&
    //row.Luminosity.HasValue &&
    row.``Highest winds``.HasValue &&
    row.``Direct fatalities``.HasValue &&
    //row.``Red Shift`` <> null &&
    //row.``Red Shift``.``Red shift``.HasValue  &&
    row.Damages <> null &&
    row.Damages.Amount.HasValue 

[ for row in rows do 
    yield (row.``Highest winds``, row.``Direct fatalities``, row.Damages) ]


let extractFeatures (row: input) = 
    [| //row.``Absolute Magnitude``.GetValueOrDefault() |> float
       //row.Age.Age.GetValueOrDefault() |> float
       //row.``Color Index``.GetValueOrDefault() |> float
       //row.Luminosity.GetValueOrDefault() |> float
       row.Mass.GetValueOrDefault() |> float
       //row.``Red Shift``.``Red shift``.GetValueOrDefault() |> float 
     |]

let extractLabel (row: input) = 
    row.``Spectral Type``.Name

[ for x in stars do 
   match x.``Spectral Type`` with 
   | null -> ()
   | b -> yield string b.Name.[0] ]

let validData = stars  |> Seq.toArray |> Array.filter isValid

let trainingData, validation = splitTraining validData

let actualResults = validation |> Array.map extractLabel

let predictor = Predictor(trainingData, extractFeatures, extractLabel)

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

validation.[0].Name // copper
validation.[1].Name // Iodine

let copper = elements.Individuals.Copper
extractFeatures copper
predictor.PredictUsingRandomForest copper
predictor.PredictUsingDecisionTree copper
copper.``Periodic table block``.Name

let iodine = elements.Individuals.Iodine
extractFeatures iodine
predictor.PredictUsingRandomForest iodine
predictor.PredictUsingDecisionTree iodine
iodine.``Periodic table block``.Name




*)
