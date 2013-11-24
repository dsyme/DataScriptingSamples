// (c) Microsoft Corporation 2005-2009. 

namespace Samples.FSharp.Math.MatrixDense

open System
open System.Collections
open System.Collections.Generic

/// The type of matrices. The arithmetic operations on the element type are determined by inspection on the element type itself.
/// All matrices are dense
[<Sealed>]
type Matrix = 

   /// Get the number of rows in a matrix
   member NumRows : int

   /// Get the number of columns in a matrix
   member NumCols : int

   /// Get the number of (rows,columns) in a matrix
   member Dimensions : int * int

   /// Get the item at the given position in a matrix
   member Item : int * int -> float with get,set

   /// Supports the slicing syntax 'A.[idx1..idx2,idx1..idx2]'
   member GetSlice : start1:int option * finish1:int option * start2:int option * finish2:int option -> Matrix

   /// Supports the slicing syntax 'A.[idx1..idx2,idx1..idx2] <- B'
   member SetSlice : start1:int option * finish1:int option * start2:int option * finish2:int option * source:Matrix -> unit

   /// Point-wise addition of two matrices. An InvalidArgument exception will be
   /// raised if the dimensions do not match.
   static member ( +  ) : Matrix * Matrix -> Matrix

   /// Point-wise subtraction of two matrices. An InvalidArgument exception will be
   /// raised if the dimensions do not match.
   static member ( -  ) : Matrix * Matrix -> Matrix

   /// Point-wise addition of two matrices. An InvalidArgument exception will be
   /// raised if the dimensions do not match.
   static member ( +  ) : Matrix * float -> Matrix

   /// Subtract each element of a matrix by the given scalar value
   static member ( -  ) : Matrix * float -> Matrix

   /// Matrix negation. 
   static member ( ~- ) : Matrix              -> Matrix

   /// Prefix '+' operator. A nop.
   static member ( ~+ ) : Matrix              -> Matrix

   /// Matrix multiplication. An InvalidArgument exception will be
   /// raised if the dimensions do not match.
   static member ( * ) : Matrix * Matrix -> Matrix

   /// Matrix-vector multiplication. 
   static member ( * ) : Matrix * Vector -> Vector

   /// Multiply each element of a matrix by the given scalar value
   static member ( * ) : Matrix * float          -> Matrix
   


   /// Point-wise matrix multiplication. An InvalidArgument exception will be
   /// raised if the dimensions do not match.
   static member ( .* ) : Matrix * Matrix  -> Matrix


   /// Multiply each element of a matrix by a scalar value
   static member ( * ) : float          * Matrix -> Matrix

   /// Divide each element of a matrix by the given scalar value
   static member ( / ) : Matrix * float          -> Matrix

   /// Point-wise matrix division. An InvalidArgument exception will be
   /// raised if the dimensions do not match.
   static member ( ./ ) : Matrix * Matrix  -> Matrix
      
   /// Get the transpose of a matrix.
   member Transpose : unit -> Matrix

   /// Permutes the rows of a matrix.
   member PermuteRows : permutation:(int -> int) -> Matrix
   
   /// Permutes the columns of a matrix.
   member PermuteColumns : permutation:(int -> int) -> Matrix

   interface IComparable
   interface IStructuralComparable
   interface IStructuralEquatable
   override GetHashCode : unit -> int
   override Equals : obj -> bool
 
   /// Return a new array containing the elements of a matrix
   member ToArray2D : unit -> float[,]  

   /// Convert a matrix to a column vector
   member ToVector : unit -> Vector 

   /// Returns sqrt(sum(norm(x)*(norm(x))) of all the elements of a matrix.
   /// The element type of a matrix must have an associated instance of INormFloat<float> (see <c>GlobalAssociations</c>) ((else NotSupportedException)).
   member Norm : float

   /// Select a column from a matrix
   member Column : index:int -> Vector

   /// Select a row from a matrix
   member Row :  index:int -> Vector

   /// Select a range of columns from a matrix
   member Columns : IList<Vector>

   /// Select a range of rows from a matrix
   member Rows : IList<Vector>

   /// Select a region from a matrix
   member Region : starti:int * startj:int * lengthi:int * lengthj:int -> Matrix

   /// Return the nth diagonal of a matrix, as a vector. Diagonal 0 is the primary
   /// diagonal, positive diagonals are further to the upper-right of a matrix.
   member GetDiagonal : int -> Vector

   /// Get the main diagonal of a matrix, as a vector
   member Diagonal : Vector

   /// Create a new matrix that is a copy of an array
   member Copy : unit -> Matrix

   member StructuredDisplayAsArray : obj

/// The type of column vectors. The arithmetic operations on the element type are determined by inspection 
/// on the element type itself
and 

 [<Sealed>]
 Vector = 


   /// Gets the number of entries in a vector
   member Length : int

   /// Gets the number of rows in a vector
   member NumRows : int

   /// Gets an item from a vector
   member Item : int -> float with get,set
   
   /// Supports the slicing syntax 'v.[idx1..idx2]'
   member GetSlice      : start:int option * finish:int option -> Vector

   /// Supports the slicing syntax 'v.[idx1..idx2] <- v2'
   member SetSlice      : start:int option * finish:int option * source:Vector -> unit

   /// Add two vectors, pointwise
   static member ( +  ) : Vector    * Vector -> Vector

   /// Subtract two vectors, pointwise
   static member ( -  ) : Vector    * Vector -> Vector

   /// Negate a vector
   static member ( ~- ) : Vector                  -> Vector

   /// Return the input vector 
   static member ( ~+ ) : Vector                  -> Vector

   /// Point-wise multiplication of two vectors.
   static member ( .* ) : Vector    * Vector -> Vector

   /// Multiply each element of a vector by a scalar value.
   static member ( * ) : float             * Vector -> Vector

   /// Multiply a vector by a scalar
   static member ( * )   : Vector    * float          -> Vector

   /// Permute the elements of a vector.
   member Permute : permutation:(int -> int) -> Vector    
   
   interface IComparable
   interface IStructuralComparable
   interface IStructuralEquatable
   interface IEnumerable<float>
   override GetHashCode : unit -> int
   override Equals : obj -> bool

   /// Return a new array containing a copy of the elements of a vector
   member ToArray : unit    -> float[]

   /// Computes the 2-norm of a vector: sqrt(x.Transpose*x).
   member Norm : float

   /// Create a new vector that is a copy of this vector
   member Copy : unit -> Vector

   member StructuredDisplayAsArray : float[]


/// The type of floating point matrices
type matrix = Matrix
/// The type of floating point column vectors
type vector = Vector

/// Operations to manipulate floating
/// point matrices. The submodule <c>Matrix.Generic</c> contains a 
/// matching set of operations to manipulate matrix types carrying
/// arbitrary element types.
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
[<RequireQualifiedAccess>]
module Matrix =


   /// Get an element of a matrix
   val get : matrix -> int * int -> float

   /// Set an element of a matrix
   val set : matrix -> int * int -> float -> unit

   // Creation: general
   val init  : int * int -> (int -> int -> float) -> matrix

   /// Create a matrix with all entries the given constant
   val create : int * int -> float -> matrix
   
   /// Create a matrix with the given entries. 
   val initDense : int * int -> seq<int * int * float> -> matrix

   /// Create a matrix with all entries zero
   val zero     : int * int          -> matrix
   
   /// Create a square matrix with the constant 1.0 lying on diagonal
   val identity      : int -> matrix

   /// Create a square matrix with a vector lying on diagonal
   val initDiagonal : vector -> matrix
   
   /// Create a matrix from the given data
   val ofList   : float list list     -> matrix

   /// Create a matrix from the given data
   val ofSeq    : seq<#seq<float>>   -> matrix

   /// Create a matrix from the given data
   val ofRows : seq<#seq<float>>   -> matrix

   /// Create a matrix from the given data
   val ofColumns : seq<#seq<float>>   -> matrix

   /// Create a matrix from the given data
   val ofArray2D : float[,]            -> matrix

   /// Return a new array containing the elements of the given matrix
   val toArray2D : matrix              -> float[,]


   /// Create a 1x1 matrix containing the given value 
   val ofScalar : float               -> matrix
   /// Create a matrix with one row from the given row vector
   val ofRowVector : vector              -> matrix
   /// Create a matrix with one column from the given vector
   val ofVector : vector              -> matrix

   /// Return the element at row 0, column 0 of a matrix
   val toScalar : matrix              -> float                
   /// Return the first row of a matrix
   val toRowVector : matrix              -> vector                
   /// Return the first column of a matrix
   val toVector : matrix              -> vector

   /// Point-wise maximum element of two matrices
   val cptMax    : matrix -> matrix -> matrix
   
   /// Point-wise minimum element of two matrices
   val cptMin    : matrix -> matrix -> matrix
   
   /// Add two matrices (operator +)
   val add       : matrix -> matrix -> matrix

   /// Dot product
   val dot       : matrix -> matrix -> float

   /// Point-wise exponential of a matrix.
   val cptPow       : matrix -> float -> matrix

   /// Transpose of a matrix. Use also m.Transpose
   val transpose     :           matrix -> matrix

   /// Sum of the diagonal elements of a matrix
   val trace     : matrix -> float
   
   /// Generate a new matrix of the same size as the input with random entries 
   /// drawn from the range 0..aij. Random numbers are generated using a globally 
   /// shared System.Random instance with the initial seed 99.
   val randomize : matrix -> matrix

   /// Sum all the elements of a matrix
   val sum       : matrix -> float

   ///Multiply all the elements of a matrix
   val prod      : matrix -> float

   ///sqrt(sum(x*x)) of all the elements of a matrix
   val norm      : matrix -> float

   /// Check if a predicate holds for all elements of a matrix
   val forall  : (float -> bool) -> matrix -> bool

   /// Check if a predicate holds for at least one element of a matrix
   val exists  : (float -> bool) -> matrix -> bool 

   /// Check if a predicate holds for all elements of a matrix
   val foralli  : (int -> int -> float -> bool) -> matrix -> bool

   /// Check if a predicate holds for at least one element of a matrix
   val existsi  : (int -> int -> float -> bool) -> matrix -> bool 

   /// Fold a function over all elements of a matrix
   val fold      : ('T -> float -> 'T) -> 'T        -> matrix -> 'T

   /// Fold an indexed function over all elements of a matrix
   val foldi      : (int -> int -> 'T -> float -> 'T) -> 'T         -> matrix -> 'T

   /// Fold a function down each column of a matrix
   val foldByCol : (float -> float -> float) -> Vector -> matrix -> Vector

   /// Fold a function along each row of a matrix
   val foldByRow : (float -> float -> float) -> Vector -> matrix -> Vector

   /// Fold a function along a particular column of a matrix
   val foldCol   : ('T -> float -> 'T) -> 'T         -> matrix -> int -> 'T 

   /// Fold a function down a particular row of a matrix
   val foldRow   : ('T -> float -> 'T) -> 'T         -> matrix -> int -> 'T
   
   /// Map a function over each element of a matrix, producing a new matrix
   val map  : (float -> float) -> matrix -> matrix
  
   /// Map the given indexed function over each element of a matrix, producing a new matrix
   val mapi  : (int -> int -> float -> float) -> matrix -> matrix
  
   /// Create a new matrix that is a copy of the given array
   val copy : matrix -> matrix
  
   /// In-place addition, mutating the first matrix argument.
   val inplaceAdd    : matrix -> matrix -> unit

   /// In-place subtraction, mutating the first matrix argument. 
   val inplaceSub    : matrix -> matrix -> unit


/// Operations to manipulate floating
/// point column vectors. The submodule VectorOps.Generic contains a 
/// matching set of operations to manipulate column vectors carrying
/// arbitrary element types.
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
[<RequireQualifiedAccess>]
module Vector =

   /// Get an element of a column vector
   val get   : vector -> int -> float

   /// Set an element of a column vector
   val set   : vector -> int -> float -> unit

   /// Get the dimensions (number of rows) of a column vector. Identical to <c>nrows</c>
   val length   : vector -> int 

   /// Create a vector of a fixed length using a function to compute the initial element values
   val init  : int ->        (int -> float)        -> vector

   /// Create a vector from a list of numbers
   val ofList   : float list          -> vector
   
   /// Create a vector from a sequence of numbers
   val ofSeq    : seq<float>         -> vector

   /// Create a vector from an array of double precision floats
   val ofArray  : float array         -> vector

   /// Return a new array containing a copy of the elements of the given vector
   val toArray  : vector           -> float array
   
   /// Create a 1-element vector
   val ofScalar  : float              -> vector

   /// Generate a vector of the given length where each entry contains the given value
   val create    : int        -> float -> vector
   
   /// Return a vector of the given length where every entry is zero.
   val zero     : int                 -> vector
   
   /// Create a vector that represents a mesh over the given range
   /// e.g. rangef (-1.0) 0.5 1.0 = vector [ -1.0; -0.5; 0.0; 0.5; 1.0]
   val rangef : float -> float -> float -> vector
   
   /// Create a vector that represents a integral mesh over the given range
   /// e.g. range 1 5 = vector [ 1.;2.;3.;4.;5. ]
   val range  : int -> int              -> vector
     
   ///Dot product
   val dot       : vector -> vector -> float

   ///Point-wise exponential of a vector.
   val cptPow    : vector -> float -> vector

   ///Sum all the elements of a vector
   val sum       : vector -> float

   ///Multiply all the elements of a matrix
   val prod      : vector -> float

   /// Computes the 2-norm of a vector: sqrt(x.Transpose*x).
   val norm      : vector -> float

   /// Return true if a predicate returns true for all values in a vector
   val forall  : (float -> bool) -> vector -> bool

   /// Return true if a predicate returns true for some value in a vector
   val exists  : (float -> bool) -> vector -> bool 

   /// Return true if an indexed predicate returns true for all values in a vector
   val foralli  : (int ->        float -> bool) -> vector -> bool

   /// Return true if an indexed predicate returns true for some value in a vector
   val existsi  : (int ->        float -> bool) -> vector -> bool 

   /// Fold a function over all elements of a vector
   val fold      : ('T -> float -> 'T) -> 'T         -> vector -> 'T

   /// Fold an indexed function over all elements of a vector
   val foldi      : (int -> 'T -> float -> 'T) -> 'T         -> vector -> 'T

   /// Copy a vector
   val copy : vector -> vector
   
   /// Map a function over each element of a vector
   val map  : (float -> float) -> vector -> vector
  
   /// Map an indexed function over each element of a vector
   val mapi  : (int        -> float -> float) -> vector -> vector
  
[<AutoOpen>]
module MatrixTopLevelOperators = 
   /// Builds a matrix from a sequence of sequence of floats.
   val matrix : seq<#seq<float>> -> matrix
   /// Builds a (column) vector from a sequence of floats.
   val vector : seq<float> -> vector
