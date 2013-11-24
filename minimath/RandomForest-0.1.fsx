module Samples.FSharp.Math.RandomForest

#load "../extlib/SeqEx-0.1.fsx"

open System
open System.Collections.Generic

type System.Random with

    /// Split a data set into training and validation sets
    member rand.SplitData(data:'T[], ?weight) = 
        let weight = defaultArg weight (2.0 / 3.0)
        let training, validation = data |> Array.partition (fun x -> rand.NextDouble() <= weight)
        training, validation

    /// Choose a sub-bag of the given sive from a data set.
    ///
    /// TODO: this is not choosing a true subset of the data - the same item may be chosen twice
    member rand.ChooseSubset(data:'T[], size) = 
        [|for _ in 1..size do yield data.[rand.Next(data.Length)]|]

let meanSdCount (xs:float[]) =
    match xs with
    | [||] -> failwith "requires at least one element"
    | [|x|] -> (x,0.,1)
    | xs ->
        let (newM, newS, n) =
            xs |> Seq.skip 1 |> Seq.fold(fun (oldM, oldS,(n:int)) x ->
                let newM = oldM + (x - oldM) / (float) n
                let newS = oldS + (x - oldM) * (x - newM)
                (newM,newS, n + 1)) (xs.[0],0.,2)
        (newM, Math.Sqrt(newS / (float) (n - 2)), n - 1)

type DecisionChoice = 
    { Feature : int
      Threshold : float
      LessThan  : DecisionTree
      MoreThan  : DecisionTree }

and DecisionTree = 
    | DecisionChoice of DecisionChoice
    | DecisionResult of int

/// Choose a feature from among the remaining features
let chooseFeature(X:float[][],y:int[],features:int[],rows:int[]) = 
    let results = rows |> Array.map (fun row -> y.[row])
    let split,feature =
          features 
          |> Array.map (fun i ->
              let (setMean,setSd,setCount) = rows |> Array.map (fun row -> X.[row].[i]) |> meanSdCount
              if setSd < Double.Epsilon then
                  (-1.,(setMean,i))
              else
                  // group by category
                  let categoryStats =
                          rows 
                              |> Seq.groupBy (fun row -> y.[row])
                              |> Seq.map (fun (c,xs) -> xs |> Seq.toArray |> Array.map (fun x -> X.[x].[i]) |> meanSdCount)
                              |> Seq.toArray

                  // Split by weighted mean
                  let split = 
                      let (means,weights) = categoryStats |> Array.map (fun (mean,sd,_) -> (mean,setSd/(setSd + sd))) |> Array.unzip
                      ((means,weights) ||> Array.map2 (fun x y -> x * y) |> Array.sum) / (weights |> Array.sum)

                  let (_,sd,_) = categoryStats |> Array.map (fun (mean,_,_) -> mean) |> meanSdCount
                  (sd / setSd,(split,i)))    
          |> Array.maxBy fst
          |> snd
    (feature,split)
        
/// features: the features to consider
let rec buildDecisionTree(X:float[][],y:int[],weights:float[],features:int[],rows:int[])  =
    let results = [ for row in rows do yield y.[row]]
    let homogenous = results |> Seq.pairwise |> Seq.exists (fun (x,y) -> x<>y) |> not
    let leafNode() =
          let category = 
              if rows.Length = 1 then y.[rows.[0]]
              else
                  results 
                      |> Seq.countBy (fun x -> x) 
                      |> Seq.maxBy (fun (x,y) -> float y * float weights.[x])
                      |> fst
          DecisionResult category
    // Single Category?
    if features.Length <= 1 || rows.Length <= 1 || homogenous then 
        leafNode()
    else 
        let (feature, threshold) = chooseFeature(X,y,features,rows)
        let lessRows = [| for row in rows do if X.[row].[feature] < threshold then yield row |]
        let moreRows = [| for row in rows do if X.[row].[feature] >= threshold then yield row |]
        if (lessRows.Length = 0 || moreRows.Length = 0) then
            leafNode()
        else
            let features' = features |> Array.filter ((<>) feature) 
            DecisionChoice
                { Feature = feature
                  Threshold = threshold
                  LessThan = buildDecisionTree(X,y,weights,features',lessRows)
                  MoreThan = buildDecisionTree(X,y,weights,features',moreRows) }
        
let rec runDecisionTree (row:float[]) (tree:DecisionTree) = 
    match tree with 
    | DecisionResult r -> r
    | DecisionChoice dc ->
        if row.[dc.Feature] < dc.Threshold 
        then runDecisionTree row dc.LessThan 
        else runDecisionTree row dc.MoreThan 

let buildRandomForest(X:float[][],y:int[],weights:float[],features:int[],treeCount:int,featureSampleCount:int,rand:Random) =   
    // Tree initialization values
    [| for _ in 0..treeCount-1 do

        // Randomly choose some features
        let features = rand.ChooseSubset(features, featureSampleCount)

        // Randomly choose 2/3 of the rows to train on. The 'validate' set is currently thrown away.
        // TODO: Wikipedia says "Use the rest of the cases to estimate the error of the tree, by predicting their classes."
        let train,_validate = rand.SplitData [| 0 .. X.Length - 1 |] 
        yield buildDecisionTree(X,y,weights,features,train) |] 

    //  Async.Paralel appears slower - perhaps try batching or Array.Parallel
    //async { return buildDecisionTree(X,y,weights,features,train)})
    //|> Async.Parallel
    //|> Async.RunSynchronously


/// <summary>A predictor that uses either a decision tree or a random forest.</summary>
/// 
/// <param name = "trainingData">the training data objects</param>
/// <param name = "extractFeatures">a function to extract the features from the training data</param>
/// <param name = "extractLabel">a function to extract the actual result label from the training data</param>
/// <param name = "treeCount">the number of trees to use in the random forest</param>
/// <param name = "featureSampleCount">the number of features to use for each tree in the random forest</param>
/// <param name = "labelWeight">the weight to assign to each label</param>
/// <param name = "rand">the random number generator to use</param>
type Predictor<'T,'Label when 'Label :equality>(trainingData:'T[],extractFeatures:('T -> float[]),extractLabel:('T -> 'Label), ?treeCount:int, ?featureSampleCount:int, ?labelWeight : ('Label -> float), ?rand:Random) = 

    let rand = defaultArg rand (Random(0))

    let treeCount = defaultArg treeCount 160

    /// Get the set of valid labels known during training
    let extractLabels rows = 
        let labels = rows |> Array.map extractLabel
        let unique = labels |> Seq.distinct  |> Seq.toArray
        let lookup1 = unique |> Seq.mapi (fun i x -> (x,i)) |> dict 
        let lookup2 = unique |> Seq.mapi (fun i x -> (i,x)) |> dict
        let labels = labels |> Array.map (fun l -> lookup1.[l])
        labels, unique.Length, (fun i -> lookup2.[i])
    
    let X = trainingData |> Array.map extractFeatures  
    let y,labelCount,labelLookup = trainingData |> extractLabels

    let featureCount = X.[0].Length

    let featureSampleCount = defaultArg featureSampleCount (int (sqrt (float featureCount)) + 1) 
    let features = [|0..featureCount - 1|] // consider all the features

    // Weight of different outcomes based on occurrence. Currently assumes equal weights.
    // TODO : check this is a reasonable default for weights among different outcomes
    let weights = 
        match labelWeight with 
        | None -> [|for i in 0 .. labelCount - 1 -> 1.0|] 
        | Some wf -> [|for i in 0 .. labelCount - 1 -> wf (labelLookup i) |] 

    let rows = X |> Array.mapi (fun i _ -> i)
    let tree = buildDecisionTree(X,y,weights,features,rows)

    // Random Forest
    let forest = buildRandomForest( X,y,weights,features,treeCount,featureSampleCount,rand)
    
    let labelThatOccursMost xs = xs |> Seq.countBy id |> Seq.maxBy snd |> fst

    member x.GetFeatureMatrix() = X
    member x.GetDecisionTree() = tree
    member x.GetRandomForest() = forest
    member x.TreeCount = treeCount

    /// Predict the label for a row using one decision tree
    member x.PredictUsingDecisionTree  row = 
        tree 
        |> runDecisionTree (extractFeatures row) 
        |> labelLookup

    /// Predict the label for a row using a random forest
    member x.PredictUsingRandomForest row = 
        forest 
        |> Array.map (runDecisionTree (extractFeatures row))  
        // Choose the result that occurs the most
        |> labelThatOccursMost
        |> labelLookup

   