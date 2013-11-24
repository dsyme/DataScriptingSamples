#load "extlib/SeqEx-0.1.fsx"
#load "minimath/NumericUtilities-0.1.fsx"
#load "data/LogisticRegressionExampleData.fsx"
#load "minimath/LogisticRegression-0.1.fsx"
#load "packages/FSharp.Charting/FSharp.Charting.fsx"

open FSharp.Charting
open FSharp.Charting.ChartTypes
open LogisticRegressionExampleData
open Samples.FSharp.Math.MatrixDense
open Samples.FSharp.Math.NumericUtilities
open Samples.FSharp.Math.LogisticRegression

// split X into the two classes
module StanfordWk2Ex1 =
    let run() =
        let (X,y) = 
                data1.Split('\n') |> Array.map (fun x -> x.Split(',') |> Array.map float) 
                    |> Matrix.ofColumns 
                    |> featureNormalize 
                    |> fun (x,_,_) -> x
                    |> fun x -> ([onesV x.NumRows;x.Column(0);x.Column(1)] |> Matrix.ofRows, [x.Column(2)] |> Matrix.ofRows)

        let iterations = 10000
        let alpha = 0.1
        let lambda = 0.

        let (Js, Thetas) = 
            regression X y alpha lambda |> Seq.take iterations |> Seq.toArray
            |> fun x -> (x |> Array.map fst, x |> Array.map snd)

        //let predict(x:float []) = ([x] |> Matrix.ofColumns) * theta
        let theta = Thetas |> Array.rev |> Seq.head

        let charts() =
            let actual = y.Column(0) |> Seq.toArray |> Array.map (fun x -> x > 0.)
            let predicted = (X * theta).Column(0) |> Seq.toArray |> Array.map (fun x -> x > 0.)
            let filter (f:int -> bool) = [for i in 0..X.NumRows-1 do if f(i) then yield (X.[i,1],X.[i,2])]
                
            let tp = filter (fun i -> predicted.[i] && actual.[i])
            let fp = filter (fun i -> predicted.[i] && not actual.[i])
            let fn = filter (fun i -> (not predicted.[i]) && actual.[i])
            let tn = filter (fun i -> (not predicted.[i]) && (not actual.[i]))

            [
              Chart.Combine 
                [
                    Chart.FastPoint(tp)
                    Chart.FastPoint(fp)
                    Chart.FastPoint(tn)
                    Chart.FastPoint(fn)
                ]
              Chart.FastLine(rocCurve((X * theta).Column(0) |> Seq.toArray, actual))
              Chart.Combine 
              Chart.FastLine(Js |> Array.mapi (fun i x -> (float i, fst x)))
            ]  : obj list
        charts()



module StanfordWk2Ex2 =
    let run() =
        let (X,y) = 
            data2.Split('\n') |> Array.map (fun x -> x.Split(',') |> Array.map float) 
                |> Matrix.ofColumns 
                |> fun x -> ([yield onesV x.NumRows; yield! ((mapFeature 6 (x.Column(0),x.Column(1))).Columns)] 
                                                                |> Matrix.ofRows
                                                                |> featureNormalize 
                                                                |> fun (x,_,_) -> x
                                                                , [x.Column(2)] |> Matrix.ofRows)
        let iterations = 500
        let alpha = 0.1
        let lambda = 0.

        let (Js, Thetas) = 
            regression X y alpha lambda |> Seq.take iterations |> Seq.toArray
            |> fun x -> (x |> Array.map fst, x |> Array.map snd)

        let theta = Thetas |> Array.rev |> Seq.head
    
        let charts() =
            let actual = y.Column(0) |> Seq.toArray |> Array.map (fun x -> x > 0.)
            let predicted = (X * theta).Column(0) |> Seq.toArray |> Array.map (fun x -> x > 0.)
            let filter (f:int -> bool) = [for i in 0..X.NumRows-1 do if f(i) then yield (X.[i,1],X.[i,2])]
                
            let tp = filter (fun i -> predicted.[i] && actual.[i])
            let fp = filter (fun i -> predicted.[i] && not actual.[i])
            let fn = filter (fun i -> (not predicted.[i]) && actual.[i])
            let tn = filter (fun i -> (not predicted.[i]) && (not actual.[i]))

            [
                Chart.Combine 
                    [
                        Chart.FastPoint(tp)
                        Chart.FastPoint(fp)
                        Chart.FastPoint(tn)
                        Chart.FastPoint(fn)
                    ]
                Chart.Line(rocCurve((X * theta).Column(0) |> Seq.toArray, actual))
                Chart.FastLine(Js |> Array.mapi (fun i x -> (float i, fst x)))
            ] : obj list 
        charts()

//StanfordWk2Ex1.run()
//StanfordWk2Ex2.run()
