#r "packages/MathNet.Numerics/lib/Net40/MathNet.Numerics.dll"
#r "packages/MathNet.Numerics.FSharp/lib/Net40/MathNet.Numerics.FSharp.dll"
#load "packages/FSharp.Charting/FSharp.Charting.fsx"
#r @"packages\MathNet.Numerics.Data.Matlab\lib\net40\MathNet.Numerics.Data.Matlab.dll"

open FSharp.Charting
open MathNet.Numerics.Distributions
open MathNet.Numerics.LinearAlgebra.Double
open MathNet.Numerics.LinearAlgebra
open MathNet.Numerics.Statistics
open Mcmc

let N = 10000
let rnd = System.Random()
let rand() = rnd.NextDouble()

module RejectionSampling = 
    printfn "Rejection Sampling Example"

    /// The target distribution.
    let beta = new Beta(2.7, 6.3)

    /// Samples uniform distributed variables.
    let uniform = ContinuousUniform(0.0, 1.0, RandomSource = rnd)

    /// Implements the rejection sampling procedure.
    let rs = RejectionSampler( pdfP=( fun x -> x**(beta.A-1.0) * (1.0 - x)**(beta.B-1.0) ),
                               pdfQ=( fun x -> 0.021 ),
                               proposal=( fun () -> uniform.Sample()) )

    /// An array of samples from the rejection sampler.
    let arr = rs.Sample(N)

    /// The true distribution.
    printfn "\tEmpirical Mean = %f (should be %f)" (Statistics.Mean arr) beta.Mean
    printfn "\tEmpirical StdDev = %f (should be %f)" (Statistics.StandardDeviation arr) beta.StdDev
    printfn "\tAcceptance rate = %f" rs.AcceptanceRate
    printfn ""



//
// Example 2: Sampling a normal distributed variable through Metropolis sampling.
//
// Target Distribution: Normal(1.0, 3.5)
//
// -----------------------------------------------------------------------------
module MetropolisSampling = 
    printfn "Metropolis Sampling Example"
   
    let mean, stddev = 1.0, 3.5
    let normal = new Normal(mean, stddev)

    /// Implements the rejection sampling procedure.
    let ms = MetropolisSampler( 0.1, pdfLnP=(fun x -> log(normal.Density(x))),
                                     proposal=(fun x -> Normal.Sample(rnd, x, 0.3)), 
                                     burnInterval=20,
                                     RandomSource = rnd )

    /// An array of samples from the rejection sampler.
    let arr = ms.Sample(N)

    /// The true distribution.
    printfn "\tEmpirical Mean = %f (should be %f)" (Statistics.Mean(arr)) normal.Mean
    printfn "\tEmpirical StdDev = %f (should be %f)" (Statistics.StandardDeviation(arr)) normal.StdDev
    printfn "\tAcceptance rate = %f" ms.AcceptanceRate
    printfn ""
   


//
// Example 3: Sampling a normal distributed variable through Metropolis-Hastings sampling
//              with a symmetric proposal distribution.
//
// Target Distribution: Normal(1.0, 3.5)
//
// -----------------------------------------------------------------------------------------
module MetropolisHastingsSamplingSymmetric = 
    printfn "Metropolis Hastings Sampling Example (Symmetric Proposal)"
    let mean, stddev = 1.0, 3.5
    let normal = Normal(mean, stddev)
   
    /// Evaluates the log normal distribution.
    let npdf x m s = -0.5*(x-m)*(x-m)/(s*s) - 0.5 * log(2.0 * System.Math.PI * s * s)

    /// Implements the rejection sampling procedure.
    let ms = MetropolisHastingsSampler( x0=0.1, 
                                        pdfLnP=(fun x -> log(normal.Density(x))), 
                                        krnlQ=(fun x y -> npdf x y 0.3), 
                                        proposal=(fun x -> Normal.Sample(rnd, x, 0.3)), 
                                        burnInterval=10,
                                        RandomSource = rnd )

    /// An array of samples from the rejection sampler.
    let arr = ms.Sample(N)

    /// The true distribution.
    printfn "\tEmpirical Mean = %f (should be %f)" (Statistics.Mean(arr)) normal.Mean
    printfn "\tEmpirical StdDev = %f (should be %f)" (Statistics.StandardDeviation(arr)) normal.StdDev
    printfn "\tAcceptance rate = %f" ms.AcceptanceRate
    printfn ""



//
// Example 4: Sampling a normal distributed variable through Metropolis-Hastings sampling
//              with a asymmetric proposal distribution.
//
// Target Distribution: Normal(1.0, 3.5)
//
// -----------------------------------------------------------------------------------------
module MetropolisHastingsSamplingAssymmetric = 
    printfn "Metropolis Hastings Sampling Example (Assymetric Proposal)"
    let mean, stddev = 1.0, 3.5
    let normal = Normal(mean, stddev)
   
    /// Evaluates the logarithm of the normal distribution function.
    let npdf x m s = -0.5*(x-m)*(x-m)/(s*s) - 0.5 * log(2.0 * System.Math.PI * s * s)
   
    /// Samples from a mixture that is biased towards samples larger than x.
    let mixSample x =
        if Bernoulli.Sample(rnd, 0.5) = 1 then
            Normal.Sample(rnd, x, 0.3)
        else
            Normal.Sample(rnd, x + 0.1, 0.3)
   
    /// The transition kernel for the proposal above.
    let krnl xnew x = log (0.5 * exp(npdf xnew x 0.3) + 0.5 * exp(npdf xnew (x+0.1) 0.3))
    /// Implements the rejection sampling procedure.
    let ms = MetropolisHastingsSampler(x0=0.1, 
                                       pdfLnP=(fun x -> log(normal.Density(x))), 
                                       krnlQ=(fun xnew x -> krnl xnew x), 
                                       proposal=(fun x -> mixSample x), 
                                       burnInterval=10,
                                       RandomSource = rnd )

    /// An array of samples from the rejection sampler.
    let arr = ms.Sample(N)

    /// The true distribution.
    printfn "\tEmpirical Mean = %f (should be %f)" (Statistics.Mean(arr)) normal.Mean
    printfn "\tEmpirical StdDev = %f (should be %f)" (Statistics.StandardDeviation(arr)) normal.StdDev
    printfn "\tAcceptance rate = %f" ms.AcceptanceRate
    printfn ""



//
// Example 5: Slice sampling a normal distributed random variable.
//
// Target Distribution: Normal(1.0, 3.5)
//
// -----------------------------------------------------------------------------------------
module SliceSampling = 
    printfn "Slice Sampling Example"
    let mean, stddev = 1.0, 3.5
    let normal = Normal(mean, stddev)
   
    /// Evaluates the unnormalized logarithm of the normal distribution function.
    let npdf x m s = -0.5*(x-m)*(x-m)/(s*s)

    /// Implements the rejection sampling procedure.
    let ms = UnivariateSliceSampler( 0.1, (fun x -> npdf x mean stddev), 5, 1.0, RandomSource = rnd )

    /// An array of samples from the rejection sampler.
    let arr = ms.Sample(N)

    /// The true distribution.
    printfn "\tEmpirical Mean = %f (should be %f)" (Statistics.Mean(arr)) normal.Mean
    printfn "\tEmpirical StdDev = %f (should be %f)" (Statistics.StandardDeviation(arr)) normal.StdDev
    printfn ""




module Unitized = 


    open Microsoft.FSharp.Data.UnitSystems.SI.UnitSymbols

    type Observation = { Time: float<s>; Location: float<m> }

    let rand() = rnd.NextDouble() 
    let randZ() = rnd.NextDouble() - 0.5

    let near p = { Time= p.Time + randZ() * 20.0<s>; Location = p.Location + randZ() * 5.0<m> }

    let data = 
         [ for i in 1 .. 1000 -> near { Time= 100.0<s>; Location = 60.0<m> }
           for i in 1 .. 1000 -> near { Time= 120.0<s>; Location = 80.0<m> }
           for i in 1 .. 1000 -> near { Time= 180.0<s>; Location = 30.0<m> }
           for i in 1 .. 1000 -> near { Time= 70.0<s>; Location = 40.0<m> } ]

    Statistics .Variance [ 0.0 .. 100.0 ]

module Misc = 

    open System.Numerics
    open MathNet.Numerics
    open MathNet.Numerics.Distributions 
    open MathNet.Numerics.Statistics
    open MathNet.Numerics.IntegralTransforms

    let data2 = seq { for i in 0 .. 10000000 -> rand()  }
    Statistics.StandardDeviation data2
    Statistics.Mean [ 0.0 .. 100.0 ]
    Statistics.Maximum [ 0.0 .. 100.0 ]
    Statistics.Minimum [ 0.0 .. 100.0 ]
    Combinatorics.Combinations(6,3)
    Combinatorics.Combinations(3,1)
    Combinatorics.Combinations(3,2)
    Combinatorics.CombinationsWithRepetition(3,1)
    Combinatorics.CombinationsWithRepetition(3,2)
    Combinatorics.CombinationsWithRepetition(3,3)
    Combinatorics.Permutations(10)
    Combinatorics.Variations(3,1)
    Combinatorics.Variations(3,2)
    //MathNet.Numerics.Control.ParallelizeOperation(100000)
    //MathNet.Numerics.Control.ParallelizeOperation(100)
    MathNet.Numerics.Distributions.Normal(10.0,4.0).Samples()
    MathNet.Numerics.Distributions.Beta(10.0,4.0).Samples()
    MathNet.Numerics.Distributions.Poisson(3.0).Samples()
    MathNet.Numerics.Distributions.Multinomial( [| 3.0; 4.0; 5.0 |], 6).Samples()
    MathNet.Numerics.Distributions.Binomial(0.5, 9).Samples()
    MathNet.Numerics.Distributions.ContinuousUniform(-100.0, 100.0).Samples()
    MathNet.Numerics.Distributions.DiscreteUniform(-100, 100).Samples()


    let exampleBellCurve = Normal(100.0, 10.0)

    let samples = exampleBellCurve.Samples()

    MathNet.Numerics.Euclid.IsPowerOfTwo 32L
    MathNet.Numerics.Euclid.IsPowerOfTwo 33L
    MathNet.Numerics.Euclid.LeastCommonMultiple (10380482L, 1203909138L)
    MathNet.Numerics.Euclid.GreatestCommonDivisor (10380482L, 1203909138L)
    let (gcd,x,y) = MathNet.Numerics.Euclid.ExtendedGreatestCommonDivisor (10380482L, 1203909138L)
    10380482L*x + 1203909138L*y = gcd

    let data3 = [ for i in 0.0 .. 0.01 .. 10.0 -> sin i ] 

    let exampleVariance = data3 |> Statistics.Variance 
    let exampleMean = data3 |> Statistics.Mean 
    let exampleMin = data3 |> Statistics.Minimum
    let exampleMax = data3 |> Statistics.Maximum


    let c1 = Complex(1.0,1.0) 
    c1 * c1
    c1 + c1 
    c1 - c1 
    c1 / c1
    //abs c1
    cos c1
    sqrt c1
    -c1
    log c1
    log10 c1
    //pown c1 3
    Complex.FromPolarCoordinates



    let complex r i = System.Numerics.Complex(r,i)


    let randomPoints = [ for i in 0 .. 1000 -> 10.0 * rand(), 10.0 * rand() ]

    Chart.Point randomPoints

    let randomTrend1 = [ for i in 0.0 ..0.1.. 10.0 -> i, sin i + rand()]
    let randomTrend2 = [ for i in 0.0 ..0.1.. 10.0 -> i, sin i + rand()]

    Chart.Combine [ Chart.Line randomTrend1; Chart.Point randomTrend2 ]

    randomPoints 
        |> fun c -> Chart.Line (c,Title="Expected Trend")


    let data4 = Fourier.NaiveForward( [| for i in 0 .. 1000 -> complex (sin (float i)) (cos (float i)) |],MathNet.Numerics.IntegralTransforms.FourierOptions.Default )
    let data5 = Fourier.NaiveForward( [| for i in 0 .. 1000 -> complex (float i) (float i) |],MathNet.Numerics.IntegralTransforms.FourierOptions.Default )
    let data6 = Fourier.NaiveForward( [| for i in 0 .. 1000 -> complex (rand()) (rand()) |],MathNet.Numerics.IntegralTransforms.FourierOptions.Default )


    let exampleBellCurve2 = Normal(100.0, 10.0)

    let histogram n data = 
        let h = Histogram(data, n)
        [| for i in 0 .. h.BucketCount - 1 -> (sprintf "%.0f-%.0f" h.[i].LowerBound h.[i].UpperBound, h.[i].Count) |]

    exampleBellCurve.Samples() |> Seq.truncate 1000 |> histogram 10 |> Chart.Column

    Chart.Point [ for i in data4 -> i.Real, i.Imaginary ] 

    Integrate.OnClosedInterval((fun i -> i * 3.0 + 4.0), 3.0, 7.0)
    Integrate.OnClosedInterval((fun i -> exp i + i * i + i * 3.0 + 4.0), 3.0, 7.0, 0.1)
    Integrate.OnClosedInterval((fun i -> exp i + i * i + i * 3.0 + 4.0), 3.0, 7.0, 0.01)
    Integrate.OnClosedInterval((fun i -> exp i + i * i + i * 3.0 + 4.0), 3.0, 7.0, 0.0000001)
    Interpolate.Common( [| for i in 0 .. 100 -> float i |],  [| for i in 0 .. 100 -> float i |]).SupportsDifferentiation
    Interpolate.Common( [| for i in 0 .. 100 -> float i |],  [| for i in 0 .. 100 -> float i |]).SupportsIntegration
    Interpolate.Common( [| for i in 0 .. 100 -> float i |],  [| for i in 0 .. 100 -> float i |]).Interpolate 3.5
    Interpolate.Linear( [| for i in 0 .. 100 -> float i |],  [| for i in 0 .. 100 -> float i |]).SupportsDifferentiation
    Interpolate.Linear( [| for i in 0 .. 100 -> float i |],  [| for i in 0 .. 100 -> float i |]).SupportsIntegration
    Interpolate.Linear( [| for i in 0 .. 100 -> float i |],  [| for i in 0 .. 100 -> float i |]).Integrate 10.0
    Interpolate.Linear( [| for i in 0 .. 100 -> float i |],  [| for i in 0 .. 100 -> float i |]).Integrate -10.0
    Interpolate.Linear( [| for i in 0 .. 100 -> (10.0) + float i |],  [| for i in 0 .. 100 -> float i |]).Integrate -10.0
    Interpolate.Linear( [| for i in 0 .. 100 -> (10.0) + float i |],  [| for i in 0 .. 100 -> float i |]).Integrate -20.0




    SpecialFunctions.Factorial 10
    SpecialFunctions.Erfc 1.0
    SpecialFunctions.Erfc 10000.0
    SpecialFunctions.Erfc 0.0
    SpecialFunctions.Erfc -1.0
    SpecialFunctions.Erfc -100.0
    Correlation.Pearson ( [ for i in 0 .. 1000 -> float i ], [ for i in 0 .. 1000 -> float i ])
    Correlation.Pearson ( [ for i in 0 .. 1000 -> rand() ], [ for i in 0 .. 1000 -> rand() ])
    Correlation.Pearson ( [ for i in 0 .. 1000 -> float i * rand() ], [ for i in 0 .. 1000 -> float i * rand() ])
    let desc = DescriptiveStatistics([ for i in 0 .. 1000 -> float i * rand() ])
    desc.Kurtosis
    desc.Maximum
    desc.Minimum
    desc.Mean
    desc.Skewness
    desc.StandardDeviation
    desc.Variance
    desc.Count

    let hist = Histogram([ for i in 0 .. 1000 -> float i * rand() ], 10)

    hist.BucketCount
    let b = hist.GetBucketOf 100.0
    b.LowerBound
    b.UpperBound
    b.Count


    //Statistics.OrderStatistic([ 0.0 .. 100.0 ], 10)

    type Vector<'T when 'T : (new : unit -> 'T) and 'T : struct and 'T :> System.ValueType and 'T :> System.IEquatable<'T> and 'T :> System.IFormattable> with 
        member v.GetSlice(a: int option, b:int option) = 
            let a = defaultArg a 0
            let b = defaultArg b (v.Count - 1)
            v.SubVector(a, b-a+1)
             
    type Matrix<'T when 'T : (new : unit -> 'T) and 'T : struct and 'T :> System.ValueType and 'T :> System.IEquatable<'T> and 'T :> System.IFormattable> with 
        member v.GetSlice(a1: int option, b1:int option, a2: int option, b2:int option) = 
            let a1 = defaultArg a1 0
            let a2 = defaultArg a2 0
            let b1 = defaultArg b1 (v.ColumnCount - 1)
            let b2 = defaultArg b2 (v.RowCount - 1)
            v.SubMatrix(a1,b1-a1+1,a2,b2-a2+1)
             
    fsi.AddPrinter (fun (x:DenseVector) -> (if x.Count > 100 then x.[0..100] else upcast x).ToString())
    fsi.AddPrinter (fun (x:DenseMatrix) -> (if x.ColumnCount > 20 || x.RowCount > 20 then x.[0..20,0..20] else upcast x).ToString())

    let svector1 = SparseVector(5)
    let svector2 = SparseVector.OfEnumerable [| 1.0; 2.0; 4.0; 3.0 |] 

    svector2.AbsoluteMinimumIndex()
    svector2.AbsoluteMaximumIndex()

    svector2.Conjugate()

    svector1.MaximumIndex()


    let vector1 = DenseVector(5)
    let vector2 = DenseVector.Create(5, (fun _ -> 3.0))
    let vector3 = DenseVector [| for i in 0 .. 10000 -> float i |] 
    let vector4 = DenseVector.OfVector(vector3)
    vector3.[4]

    matrix 


    fsi.AddPrintTransformer (fun (x:DenseMatrix) -> box (array2D [ for i in 0 .. x.RowCount-1 -> [ for j in 0 .. x.ColumnCount - 1 -> x.[i,j] ] ]))
    fsi.AddPrintTransformer (fun (x:DenseVector) -> box [| for i in 0 .. x.Count-1 -> x.[i] |])

    let vector5 = vector [ 1.0; 2.4 ; 3.0 ] 
    let vector6 = vector [ 7.0; 2.1 ; 5.4 ]

    vector5 + vector6

    let matrix1 = 
        matrix [ [ 1.0; 2.0 ]
                 [ 1.0; 3.0 ] ]

    let matrix2 = 
        matrix [ [ 1.0; -2.0 ]
                 [ 0.5; 3.0 ] ]

    matrix1 * matrix2


    let largeMatrix = matrix [ for i in 1 .. 100 -> [ for j in 1 .. 100 -> rand() ] ]

    let result = largeMatrix * largeMatrix.Inverse()

    let svd = largeMatrix.Svd(true)
    let evd = largeMatrix.Evd()

    evd.EigenValues

    fsi.AddPrinter (fun (c:System.Numerics.Complex) -> sprintf "%fr + %fi" c.Real c.Imaginary)


    largeMatrix.Determinant()

    matrix [ for i in 1 .. 100 -> [ for j in 1 .. 100 -> float (i+j) ] ]
    let largeMatrix1 = DenseMatrix.OfArray(array2D [| for i in 1 .. 100 -> [| for j in 1 .. 100 -> float (i+j) |] |])
    let largeMatrix2 = DenseMatrix.OfArray(array2D [| for i in 1 .. 100 -> [| for j in 1 .. 100 -> rand() |] |])
    array2D [| for i in 1 .. 100 -> [| for j in 1 .. 100 -> rand() |] |]


    let writeMatlabMatrix (m: Matrix<_>, fileName: string) = 
        MathNet.Numerics.Data.Matlab.MatlabWriter.Write(fileName, largeMatrix, "TheMatrix")

    let readMatlabMatrix (fileName:string) = 
        MathNet.Numerics.Data.Matlab.MatlabReader.Read(fileName, "TheMatrix")
        
        
    writeMatlabMatrix (largeMatrix, @"c:\misc\a2.out")
    let result3 : Matrix<float> = readMatlabMatrix @"c:\misc\a2.out" 




