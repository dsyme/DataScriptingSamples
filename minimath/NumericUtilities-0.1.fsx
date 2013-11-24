
namespace Samples.FSharp.Math

#load "MatrixDense-0.1.fsi" "MatrixDense-0.1.fs" 
#load "../extlib/ArrayEx-0.1.fsx"

//#load "MatrixSparseDense.fsi" "MatrixSparseDense.fs"
open Samples.FSharp.Math.MatrixDense
open System


[<AutoOpen>]                 
module NumericUtilities =
    let zeros(shape) = Matrix.init shape (fun _ _ -> 0.)
    let ones(shape) = Matrix.init shape (fun _ _ -> 1.)
    let onesV(n:int) = Vector.init n (fun _ -> 1.)
    let eye(shape) = Matrix.init shape (fun x y -> if x = y then 1. else 0.)
    let ofArray(shape:int*int) (xss:float[][]) = Matrix.init shape (fun x y -> xss.[x].[y])
    let reduce (f:float[]->float) (a:Matrix) : Vector =
        [|0..a.NumCols - 1|] 
            |> Array.map (fun i -> a.Column(i).ToArray() |> f )
            |> Vector.ofArray
    /// Make
    let rep(v:Vector, i:int) = Matrix.init (i,v.Length) (fun x y -> v.[y])
    let std = reduce (fun xs -> ((xs |> Array.mapReduce (fun x -> x*x) (+)) / float xs.Length) |> sqrt)
    let median (X:Matrix) = reduce (fun xs ->   let sorted = xs |> Array.sort 
                                                if xs.Length % 2 = 0
                                                then (xs.[xs.Length / 2] + xs.[xs.Length / 2 + 1]) / 2.
                                                else xs.[xs.Length / 2]) X

    let sum = reduce (fun xs -> (xs |> Array.reduce (+)))
    let sum2 (xs:Matrix) = xs |> Matrix.sum
    let mean = reduce (fun xs -> (xs |> Array.reduce (+)) / (float xs.Length))
    let exp (X:Matrix) = X |> Matrix.map (fun x -> Math.Exp(x))
    let log (X:Matrix) = X |> Matrix.map (fun x -> Math.Log(x))
    
    /// Transpose
    let tr (a:Matrix) = a.Transpose()
    
    let (.^) (X:Vector) (y:float) = X |> Vector.map (fun x -> Math.Pow(x,y))
    
    /// g = (exp(z .* -1) .+ 1) .^ -1;
    let sigmoid(X:matrix) = ones(X.Dimensions) ./ (exp((X * -1.0)) + 1.)

    let maxi(X:matrix) : float[]*int[] =
        [|0..X.NumRows|]
            |> Array.map (fun i -> X.Row(i).ToArray() |> Array.maxi)
            |> fun xs -> (xs |> Array.map fst, xs |> Array.map snd)
    let cons(v:Vector,X:Matrix) =
        [yield v;yield! X.Columns] |> Matrix.ofColumns

    let featureNormalize (X:Matrix) =
        let μ = mean(X)
        let σ = std(X)
        let X' = (X - rep(μ,X.NumRows)) ./ rep(σ,X.NumRows)
        (X',μ,σ)

    let analyze(predicted:bool[],actual:bool[]) =
        let count xs = xs |> Array.map (fun p -> if p then 1 else 0) |> Array.sum
        let tp = (predicted,actual) ||> Array.map2 (&&) |> count 
        let fp = (predicted,actual) ||> Array.map2 (fun x y -> x && not y) |> count
        let fn = (predicted,actual) ||> Array.map2 (fun x y -> (not x) && y) |> count
        let tn = (predicted,actual) ||> Array.map2 (fun x y -> (not x) && (not y)) |> count
        (tp,fp,fn,tn)

    let summarize(tp,fp,fn,tn) =
        printfn "True Positive: \t%i\nFalse Positive: \t%i\nFalse Negative: \t%i\nTrue Negative: \t%i" tp fp fn tn
        printfn "Precision tp/(tp+fp): %f" (float tp / (float tp + float fp))
        printfn "Accuracy (tp + tn)/all: %f" ((float tp + float tn) / (float tp + float fp + float tn + float fn))

    let rocCurve(xs:float[],ys:bool[]) =
        [0..200] 
        |> List.map (fun i ->
            let p = float i / 100. - 1.
            let (tp,fp,fn,tn) = analyze(xs |> Array.map (fun x -> x > p), ys)
            (float fp / float (fp + tn), float tp/ float (tp + fn)))

    let length (xs:float[]) = Math.Sqrt((xs,xs) ||> Array.map2 (*) |> Array.sum)
    let crossProduct(xs:float[], ys:float[]) = Array.map2 (*) xs ys |> Array.sum
    let projection(v:float[],u:float[]) = 
                let x = (crossProduct(v,u) / crossProduct(u,u))
                u |> Array.map ((*) x)





   
        

