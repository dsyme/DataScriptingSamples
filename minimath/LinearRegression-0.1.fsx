module Samples.FSharp.Math.LinearRegression

#load "NumericUtilities-0.1.fsx"
#load "../extlib/SeqEx-0.1.fsx"

open Samples.FSharp.Math.MatrixDense
open NumericUtilities
open System.IO

(* TODO - WARNING does not yet take into account gradient !!!! *)

/// Compute Cost
let computeCost(X:Matrix, theta:Matrix, y:Matrix) : float = 
    //J = sum(sum(((X*theta) - y)' * ((X*theta) - y))) / (2 * m);
    let x = X * theta - y
    (tr(x) * x |> sum2) / (2.0 * float X.NumRows)
    
    
/// Gradient Descent Step
let step (X:Matrix) (y:Matrix) (alpha:float) (theta:Matrix) : Matrix = 
    //theta = theta - (alpha / m) .* (X' * ((X*theta) -y))
    theta - (((tr(X) * (X * theta - y)) * (alpha / float X.NumRows)))
    
/// Linear Regression
let regression (X:Matrix) (y:Matrix) (alpha:float) =
    let theta = zeros(X.NumCols, y.NumCols)
    Seq.iterate (step X y alpha) theta
        |> Seq.map (fun theta -> computeCost(X,theta,y),theta)