namespace Samples.FSharp.Math.LogisticRegression

#load "NumericUtilities-0.1.fsx"
#load "../extlib/SeqEx-0.1.fsx"

open Samples.FSharp.Math.MatrixDense
open Samples.FSharp.Math.NumericUtilities


[<AutoOpen>]
module LogisticRegression =
    /// CostFunctionReg
    let costFunctionReg(X:Matrix,y:Matrix, theta:Matrix, lambda:float) =
        //m = length(y); 
        let m = float X.NumRows
        // theta_prime = theta;
        let theta_prime = theta 
        //theta_prime(1) = 0;
        theta_prime.[0,0] <- 0.;
        //gz = sigmoid(X * theta);
        let gz = sigmoid(X * theta)
        // J = (sum((-y .* log(gz)) - ((1 - y) .* log(1 - gz))) / m) + (sum(theta_prime .^ 2) * (lambda / (2 * m))) ;
        let J = sum2((-y .* log(gz)) - ((-y + 1.) .* log(-gz + 1.))) + (sum2(theta_prime .* theta_prime) * (lambda / (2. * float X.NumRows)))
        //grad = (((gz - y)' * X)' ./ m) + (theta_prime .* (lambda / m));
        let grad = (tr(tr(gz - y) * X) * (1. / m)) + (theta_prime * (lambda / m))
        (J,grad)


    let mapFeature (degree:int) (x1:Vector,x2:Vector)  =
        [|for i in 1..degree do
            for j in 0..i do
                // out(:, end+1) = (X1.^(i-j)).*(X2.^j);
                yield (x1 .^ (float (i - j))) .* (x2 .^ float j)|]
        |> Matrix.ofRows // todo check this...

    /// Gradient Descent Step
    let step (X:Matrix) (y:Matrix) (alpha:float) (theta:Matrix) : Matrix = 
        //theta = theta - (alpha / m) .* (X' * ((X*theta) -y))
        theta - (((tr(X) * (X * theta - y)) * (alpha / float X.NumRows)))
    
    /// Linear Regression
    let regression (X:Matrix) (y:Matrix) (alpha:float) (lambda:float) =
        let theta = zeros(X.NumCols, y.NumCols)
        Seq.iterate (step X y alpha) theta 
            |> Seq.map (fun theta -> costFunctionReg(X,y,theta,lambda),theta)


