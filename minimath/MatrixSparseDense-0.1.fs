namespace Samples.FSharp.Math.MatrixSparseDense

   #nowarn "60"
   #nowarn "69"

   open System
   open System.Globalization
   open System.Collections
   open System.Collections.Generic
   open System.Diagnostics
   type permutation = int -> int

   [<AutoOpen>]
   module Helpers = 
       let sparseNYI() = failwith "this operation is not supported on sparse matrices"
       let sparseNotMutable() = failwith "sparse matrices are not mutable"

       [<Literal>]
       let DenseMaxDisplay = 50

       [<Literal>]
       let VectorMaxDisplay = 100
   
       let noMutation() = raise (NotSupportedException("The collection may not be mutated"))
       let noSearch() = raise (NotSupportedException("The collection may be searched"))
       let IndexedList m n f =    
          { new IList<'T> with 
                member s.Add(x) = noMutation()
                member s.Clear() = noMutation()
                member s.Remove(x) = noMutation()
                member s.RemoveAt(x) = noMutation()
                member s.IndexOf(x) = noMutation()
                member s.Insert(n,x) = noMutation()
                member s.Contains(x) = noSearch()
                member s.CopyTo(arr,start) = for i = 0 to n m - 1 do arr.[start+i] <- f m i done
                member s.IsReadOnly = true
                member s.Count = n m
                member s.Item with get i = f m i and set i v = noMutation()
            interface IEnumerable<'T> with
                  member s.GetEnumerator() = (seq { for i = 0 to n m - 1 do yield f m i }).GetEnumerator()
            interface System.Collections.IEnumerable with
                  member s.GetEnumerator() = (seq { for i = 0 to n m - 1 do yield f m i } :> System.Collections.IEnumerable).GetEnumerator() }
   
   type DenseMatrix(values : float[,]) = 
       member m.Values =  values
       member m.NumRows = values.GetLength(0)
       member m.NumCols = values.GetLength(1)

       member m.Item
          with get (i,j) = values.[i,j]
          and  set (i,j) x = values.[i,j] <- x

   type SparseMatrix(sparseValues : float[], sparseRowOffsets : int[], ncols:int, columnValues: int[]) = 
       member m.NumCols = ncols
       member m.NumRows = sparseRowOffsets.Length - 1
       member m.SparseColumnValues = columnValues
       member m.SparseRowOffsets =  sparseRowOffsets (* nrows + 1 elements *)
       member m.SparseValues =  sparseValues

       member m.MinIndexForRow i = m.SparseRowOffsets.[i]
       member m.MaxIndexForRow i = m.SparseRowOffsets.[i+1]
             

       member m.Item 
           with get (i,j) = 
               let imax = m.NumRows
               let jmax = m.NumCols
               if j < 0 || j >= jmax || i < 0 || i >= imax then raise (new System.ArgumentOutOfRangeException()) else
               let kmin = m.MinIndexForRow i
               let kmax = m.MaxIndexForRow i
               let rec loopRow k =
                   (* note: could do a binary chop here *)
                   if k >= kmax then 0.0 else
                   let j2 = columnValues.[k]
                   if j < j2 then 0.0 else
                   if j = j2 then sparseValues.[k] else 
                   loopRow (k+1)
               loopRow kmin

#if FX_NO_DEBUG_DISPLAYS
#else
   [<System.Diagnostics.DebuggerDisplay("{DebugDisplay}")>]
#endif
   [<StructuredFormatDisplay("matrix {StructuredDisplayAsArray}")>]
   [<CustomEquality; CustomComparison>]
   //[<System.Diagnostics.DebuggerTypeProxy(typedefof<MatrixDebugView>)>]
   type Matrix = 
       | DenseRepr of DenseMatrix
       | SparseRepr of SparseMatrix
       interface System.IComparable
       interface IStructuralComparable
       interface IStructuralEquatable

       member m.NumRows    = match m with DenseRepr mr -> mr.NumRows    | SparseRepr mr ->  mr.NumRows
       member m.NumCols    = match m with DenseRepr mr -> mr.NumCols    | SparseRepr mr ->  mr.NumCols

       member m.Item 
           with get (i,j) = 
               match m with 
               | DenseRepr dm -> dm.[i,j]
               | SparseRepr sm -> sm.[i,j]
           and set (i,j) x = 
             match m with 
             | DenseRepr dm -> dm.[i,j] <- x
             | SparseRepr _ -> sparseNotMutable()


#if FX_NO_DEBUG_DISPLAYS
#else
       [<DebuggerBrowsable(DebuggerBrowsableState.Collapsed)>]
#endif
       member m.IsDense = match m with DenseRepr _ -> true | SparseRepr _ -> false

#if FX_NO_DEBUG_DISPLAYS
#else
       [<DebuggerBrowsable(DebuggerBrowsableState.Collapsed)>]
#endif
       member m.IsSparse = match m with DenseRepr _ -> false | SparseRepr _ -> true

#if FX_NO_DEBUG_DISPLAYS
#else
       [<DebuggerBrowsable(DebuggerBrowsableState.Collapsed)>]
#endif
       member m.InternalSparseColumnValues = match m with DenseRepr _ -> invalidOp "not a sparse matrix" | SparseRepr mr -> mr.SparseColumnValues

#if FX_NO_DEBUG_DISPLAYS
#else
       [<DebuggerBrowsable(DebuggerBrowsableState.Collapsed)>]
#endif
       member m.InternalSparseRowOffsets = match m with DenseRepr _ -> invalidOp "not a sparse matrix" | SparseRepr mr -> mr.SparseRowOffsets

#if FX_NO_DEBUG_DISPLAYS
#else
       [<DebuggerBrowsable(DebuggerBrowsableState.Collapsed)>]
#endif
       member m.InternalSparseValues = match m with DenseRepr _ -> invalidOp "not a sparse matrix" | SparseRepr mr -> mr.SparseValues

#if FX_NO_DEBUG_DISPLAYS
#else
       [<DebuggerBrowsable(DebuggerBrowsableState.Collapsed)>]
#endif
       member m.InternalDenseValues = match m with DenseRepr mr -> mr.Values | SparseRepr _ -> invalidOp "not a dense matrix"


#if FX_NO_DEBUG_DISPLAYS
#else
   [<System.Diagnostics.DebuggerDisplay("{DebugDisplay}")>]
#endif
#if FX_NO_DEBUG_PROXIES
#else
   [<System.Diagnostics.DebuggerTypeProxy(typedefof<VectorDebugView>)>]
#endif
   [<StructuredFormatDisplay("vector {StructuredDisplayAsArray}")>]
   [<Sealed>]
   type Vector(arrV : float[]) =

#if FX_NO_DEBUG_DISPLAYS
#else
       [<DebuggerBrowsable(DebuggerBrowsableState.Collapsed)>]
#endif
       member x.InternalValues = arrV
       member x.Values = arrV
       interface System.IComparable
       interface IStructuralComparable
       interface IStructuralEquatable 

       interface IEnumerable<float> with 
           member x.GetEnumerator() = (arrV :> seq<float>).GetEnumerator()
       interface IEnumerable  with 
           member x.GetEnumerator() = (arrV :> IEnumerable).GetEnumerator()
       

       member m.Length = arrV.Length
       member m.NumRows = arrV.Length
       member v.Item
          with get i = arrV.[i]
          and  set i x = arrV.[i] <- x

#if FX_NO_DEBUG_PROXIES
#else
   and 
       [<Sealed>]
       VectorDebugView(v: Vector)  =  

            [<System.Diagnostics.DebuggerBrowsable(System.Diagnostics.DebuggerBrowsableState.RootHidden)>]
            member x.Items = v |> Seq.truncate 1000 |> Seq.toArray 
#endif


   /// Implementations of operations that will work for any type
   module Impl = 

       type smatrix = SparseMatrix
       type dmatrix = DenseMatrix
       type vector = Vector

       let mkDenseMatrix arr = DenseMatrix(arr)
       let mkVec arr = Vector(arr)

       let inline getArray2D  (arr : _[,]) i j   = arr.[i,j]
       let inline setArray2D  (arr  : _[,]) i j x = arr.[i,j] <- x

       let inline createArray m = Array.zeroCreate m

       let inline createArray2D m n = Array2D.zeroCreate m n

       let inline assignArray2D m n f arr =  
           for i = 0 to m - 1 do 
               for j = 0 to n - 1 do 
                   (arr  : _[,]).[i,j] <- f i j

       let inline assignDenseMatrix f (a:DenseMatrix) = 
           assignArray2D a.NumRows a.NumCols f a.Values
       
       let inline assignArray m f (arr : _[]) = 
           for i = 0 to m - 1 do 
               arr.[i] <- f i

       let inline assignVec f (a:Vector) = 
           assignArray a.NumRows f a.Values
              
       let inline createDenseMatrix m n f = (* inline eliminates unknown f call *)
           let arr = createArray2D m n 
           assignArray2D m n f arr;
           DenseMatrix(arr)
       
       let inline createVec m f = (* inline eliminates unknown f call *)
           let arr = createArray m 
           assignArray m f arr;
           mkVec arr

       /// Create a matrix from a sparse sequence 
       let initSparseMatrix maxi maxj s = 

           (* nb. could use sorted dictionary but that is in System.dll *)
           let tab = Array.create maxi null
           let count = ref 0
           for (i,j,v) in s do
               if i < 0 || i >= maxi || j <0 || j >= maxj then failwith "initial value out of range";
               count := !count + 1;
               let tab2 = 
                   match tab.[i] with 
                   | null -> 
                       let tab2 = new Dictionary<_,_>(3) 
                       tab.[i] <- tab2;
                       tab2
                   | tab2 -> tab2
               tab2.[j] <- v
           // optimize this line....
           let offsA =  
              let rowsAcc = Array.zeroCreate (maxi + 1)
              let mutable acc = 0 
              for i = 0 to maxi-1 do 
                 rowsAcc.[i] <- acc;
                 acc <- match tab.[i] with 
                         | null -> acc
                         | tab2 -> acc+tab2.Count
              rowsAcc.[maxi] <- acc;
              rowsAcc
              
           let colsA,valsA = 
              let colsAcc = new ResizeArray<_>(!count)
              let valsAcc = new ResizeArray<_>(!count)
              for i = 0 to maxi-1 do 
                 match tab.[i] with 
                 | null -> ()
                 | tab2 -> tab2 |> Seq.toArray |> Array.sortBy (fun kvp -> kvp.Key) |> Array.iter (fun kvp -> colsAcc.Add(kvp.Key); valsAcc.Add(kvp.Value));
              colsAcc.ToArray(), valsAcc.ToArray()

           SparseMatrix(sparseValues=valsA, sparseRowOffsets=offsA, ncols=maxj, columnValues=colsA)
       
       let listDenseMatrix xss =
           let m = List.length xss
           match xss with 
           | [] -> invalidArg "xss" "unexpected empty list"
           | h :: t -> 
             let n = List.length h
             if not (List.forall (fun xs -> List.length xs=n) t) then invalidArg "xss" "the lists are not all of the same length";
             let values = Array2D.zeroCreate m n
             List.iteri (fun i rw -> List.iteri (fun j x -> values.[i,j] <- x) rw) xss;
             DenseMatrix(values)
       
       let listVec xs = mkVec (Array.ofList xs) 

       let seqDenseMatrix xss = listDenseMatrix (xss |> Seq.toList |> List.map Seq.toList)
       let seqVec  xss = listVec (xss |> Seq.toList)

       let inline binaryOpDenseMatrix f (a:DenseMatrix) (b:DenseMatrix) = (* pointwise binary operator *)
           let nA = a.NumCols
           let mA = a.NumRows
           let nB = b.NumCols 
           let mB = b.NumRows
           if nA<>nB || mA<>mB then invalidArg "a" "the two matrices do not have compatible dimensions";
           let arrA = a.Values 
           let arrB = b.Values 
           createDenseMatrix mA nA (fun i j -> f (getArray2D arrA i j) (getArray2D arrB i j))


       let nonZeroEntriesSparseMatrix  (a:SparseMatrix) = 
           // This is heavily used, and this version is much faster than
           // the sequence operators.
           let entries = new ResizeArray<_>(a.SparseColumnValues.Length)
           let imax = a.NumRows
           for i in 0 .. imax - 1 do
             let kmin = a.MinIndexForRow i
             let kmax = a.MaxIndexForRow i
             for k in kmin .. kmax - 1 do
                 let j = a.SparseColumnValues.[k]
                 let v = a.SparseValues.[k]
                 if not (v = 0.0) then
                   entries.Add((i,j,v))
           (entries :> seq<_>)

       let nonzeroEntriesDenseMatrix  (a:DenseMatrix) = 
           let imax = a.NumRows
           let jmax = a.NumCols
           seq { for i in 0 .. imax - 1 do 
                   for j in 0 .. jmax - 1 do 
                       let v = a.[i,j] 
                       if not (v = 0.0) then
                            yield (i,j,v) }


       // pointwise operation on two sparse matrices. f must be zero-zero-preserving, i.e. (f 0 0 = 0) 
       let binaryOpSparseMatrix f (a:SparseMatrix) (b:SparseMatrix) = 
           let imax1 = a.NumRows  
           let imax2 = b.NumRows
           let jmax1 = a.NumCols
           let jmax2 = b.NumCols
           if imax1 <> imax2 || jmax1 <> jmax2 then invalidArg "b" "the two matrices do not have compatible dimensions";
           let imin = 0
           let imax = imax1
           let jmax = jmax1
           let rowsR = Array.zeroCreate (imax+1)
           let colsR = new ResizeArray<_>(max a.SparseColumnValues.Length b.SparseColumnValues.Length)
           let valsR = new ResizeArray<_>(max a.SparseValues.Length b.SparseValues.Length)
           let rec loopRows i  = 
               rowsR.[i] <- valsR.Count;            
               if i >= imax1 then () else
               let kmin1 = a.MinIndexForRow i
               let kmax1 = a.MaxIndexForRow i 
               let kmin2 = b.MinIndexForRow i
               let kmax2 = b.MaxIndexForRow i
               let rec loopRow k1 k2  =
                   if k1 >= kmax1 && k2 >= kmax2 then () else
                   let j1 = if k1 >= kmax1 then jmax else a.SparseColumnValues.[k1]
                   let j2 = if k2 >= kmax2 then jmax else b.SparseColumnValues.[k2]
                   let v1 = if j1 <= j2 then a.SparseValues.[k1] else 0.0
                   let v2 = if j2 <= j1 then b.SparseValues.[k2] else 0.0
                   let jR = min j1 j2
                   let vR = f v1 v2
                   (* if vR <> zero then  *)
                   colsR.Add(jR);
                   valsR.Add(vR);
                   loopRow (if j1 <= j2 then k1+1 else k1) (if j2 <= j1 then k2+1 else k2)
               loopRow kmin1 kmin2;
               loopRows (i+1) 
           loopRows imin;
           SparseMatrix(sparseRowOffsets=rowsR, 
                        ncols= a.NumCols, 
                        columnValues=colsR.ToArray(), 
                        sparseValues=valsR.ToArray())

       let inline binaryOpVec f (a:Vector) (b:Vector) = (* pointwise binary operator *)
           let mA = a.NumRows
           let mB = b.NumRows
           if mA<>mB then invalidArg "b" "the two vectors do not have compatible dimensions"
           createVec mA (fun i -> f a.[i] b.[i])

       let inline unaryOpDenseMatrix f (a:DenseMatrix) =
           let nA = a.NumCols 
           let mA = a.NumRows 
           let arrA = a.Values 
           createDenseMatrix mA nA (fun i j -> f (getArray2D arrA i j))


       let inline unaryOpVector f (a:Vector) =
           let mA = a.NumRows 
           let arrA = a.Values 
           createVec mA (fun i -> f arrA.[i])

       let unaryOpSparse f (a:SparseMatrix) = (* pointwise zero-zero-preserving binary operator (f 0 = 0) *)
           SparseMatrix(sparseRowOffsets=Array.copy a.SparseRowOffsets, 
                        columnValues=Array.copy a.SparseColumnValues, 
                        sparseValues=Array.map f a.SparseValues, 
                        ncols=a.NumCols)

       // Strictly speaking, sparse arrays are non mutable so no copy is ever needed. But implementing it 
       // anyway in case we move to mutability 
       let copySparse (a:SparseMatrix) = 
           SparseMatrix(sparseRowOffsets=Array.copy a.SparseRowOffsets, 
                        columnValues=Array.copy a.SparseColumnValues,
                        sparseValues=Array.copy a.SparseValues, 
                        ncols=a.NumCols)

       let inline foldR f z (a,b) = 
           let mutable res = z in
           for i = a to b do
               res <- f res i
           res

       let inline sumfR f (a,b) =
           let mutable res = 0.0 
           for i = a to b do
               res <- res + f i
           res
         
       let inline sumR f r = 
           r |> foldR (fun z k -> z + (f k)) 0.0

       let debug = false
       
       // SParse matrix multiplication algorithm. inline to get specialization at the 'double' type
       let inline genericMulSparse zero add mul (a:SparseMatrix) (b:SparseMatrix) =
           let nA = a.NumCols
           let mA = a.NumRows
           let nB = b.NumCols 
           let mB = b.NumRows
           if nA<>mB then invalidArg "b" "the two matrices do not have compatible dimensions"
           let C = new ResizeArray<_>()
           let jC = new ResizeArray<_>()
           let ma1 = mA + 1 
           let offsAcc = Array.zeroCreate ma1
           let index = Array.zeroCreate mA
           let temp = Array.create mA zero
           let ptr = new Dictionary<_,_>(11)
           if debug then printf "start, #items in result = %d, #offsAcc = %d, mA = %d\n" jC.Count offsAcc.Length mA;

           let mutable mlast = 0
           for i = 0 to mA-1 do
               if debug then printf "i = %d, mlast = %d\n" i mlast;
               offsAcc.[i] <- mlast
               
               let kmin1 = a.MinIndexForRow i
               let kmax1 = a.MaxIndexForRow i
               if kmin1 < kmax1 then 
                   let mutable itemp = 0
                   let mutable ptrNeedsClear = true // clear the ptr table on demand. 
                   for j = kmin1 to kmax1 - 1 do
                       if debug then printf "  j = %d\n" j;
                       let ja_j = a.SparseColumnValues.[j]
                       let kmin2 = b.MinIndexForRow ja_j
                       let kmax2 = b.MaxIndexForRow ja_j
                       for k = kmin2 to kmax2 - 1 do
                           let jb_k = b.SparseColumnValues.[k]
                           if debug then printf "    i = %d, j = %d, k = %d, ja_j = %d, jb_k = %d\n" i j k ja_j jb_k;
                           let va = a.SparseValues.[j] 
                           let vb = b.SparseValues.[k]
                           if debug then printf "    va = %O, vb = %O\n" va vb;
                           let summand = mul va vb
                           if debug then printf "    summand = %O\n" summand;
                           if ptrNeedsClear then (ptr.Clear();ptrNeedsClear <- false);

                           if not (ptr.ContainsKey(jb_k)) then
                               if debug then printf "    starting entry %d\n" jb_k;
                               ptr.[jb_k] <- itemp
                               let ptr_jb_k = itemp
                               temp.[ptr_jb_k] <- summand
                               index.[ptr_jb_k] <- jb_k
                               itemp <- itemp + 1
                           else
                               if debug then printf "    adding to entry %d\n" jb_k;
                               let ptr_jb_k = ptr.[jb_k]
                               temp.[ptr_jb_k] <- add temp.[ptr_jb_k] summand
                       done
                   done
                   if itemp > 0 then 
                       // Sort by index. 
                       // REVIEW: avoid the allocations here
                       let sorted = (temp.[0..itemp-1],index.[0..itemp-1]) ||> Array.zip 
                       Array.sortInPlaceBy (fun (_,idx) -> idx) sorted
                       for s = 0 to itemp-1 do
                           let (v,idx) = sorted.[s]
                           if debug then printf "  writing value %O at index %d to result matrix\n" v idx;
                           C.Add(v)
                           jC.Add(idx)
                       if debug then printf " itemp = %d, mlast = %d\n" itemp mlast;
                       mlast <- mlast + itemp 
           done
           offsAcc.[mA] <- mlast;
           if debug then printf "done, #items in result = %d, #offsAcc = %d, mA = %d\n" jC.Count offsAcc.Length mA;
           SparseMatrix(sparseRowOffsets=offsAcc,
                        ncols= nB,
                        columnValues=jC.ToArray(),
                        sparseValues=C.ToArray())

       let mulSparseVec (a:SparseMatrix) (b:Vector) =
           let nA = a.NumCols 
           let mA = a.NumRows 
           let mB    = b.NumRows 
           if nA<>mB then invalidArg "b" "the two inputs do not have compatible dimensions"
           createVec mA (fun i -> 
               let mutable acc = 0.0
               for k = a.MinIndexForRow i to a.MaxIndexForRow i - 1 do
                   let j = a.SparseColumnValues.[k]
                   let v = a.SparseValues.[k] 
                   acc <- acc + (v * b.[j]);
               acc)


       let mapDenseMatrix f (a : DenseMatrix) : DenseMatrix = 
           let arrA = a.Values 
           createDenseMatrix a.NumRows a.NumCols (fun i j -> f (getArray2D arrA i j))

       let mapV f (a:Vector) = 
           let mA= a.NumRows
           createVec mA (fun i -> f a.[i])

       let copyDenseMatrix (a : DenseMatrix) : DenseMatrix = 
           let arrA = a.Values 
           createDenseMatrix a.NumRows a.NumCols (fun i j -> getArray2D arrA i j)

       let copyV (a:Vector) = 
           createVec a.NumRows (fun i -> a.[i])

       let toDenseSparseMatrix (a:SparseMatrix) = 
           createDenseMatrix a.NumRows a.NumCols  (fun i j -> a.[i,j])
         
       let mapiDenseMatrix f (a: DenseMatrix) : DenseMatrix = 
           let arrA = a.Values 
           createDenseMatrix a.NumRows a.NumCols (fun i j -> f i j (getArray2D arrA i j))

       let mapiV f (a:Vector) = 
           createVec a.NumRows (fun i -> f i a.[i])

       let permuteV (p:permutation) (a:Vector) = 
           createVec a.NumRows (fun i -> a.[p i])

       let inline inplace_mapiDenseMatrix f (a:DenseMatrix) = 
           let arrA = a.Values 
           assignDenseMatrix (fun i j -> f i j (getArray2D arrA i j)) a

       let inline inplace_mapiVec f (a:Vector) = 
           assignVec (fun i -> f i a.[i]) a

       let inline foldDenseMatrix f z (a:DenseMatrix) =
           let nA = a.NumCols 
           let mA = a.NumRows
           let arrA = a.Values 
           let mutable acc = z
           for i = 0 to mA-1 do
               for j = 0 to nA-1 do 
                  acc <- f acc (getArray2D arrA i j)
           acc
       
       let inline foldVec f z (a:Vector) =
           let mutable acc = z
           for i = 0 to a.NumRows-1 do acc <- f acc a.[i]
           acc
       
       let inline foldiDenseMatrix f z (a:DenseMatrix) =
           let nA = a.NumCols 
           let mA = a.NumRows
           let arrA = a.Values 
           let mutable acc = z
           for i = 0 to mA-1 do
               for j = 0 to nA-1 do 
                  acc <- f i j acc (getArray2D arrA i j)
           acc
       
       let inline foldiVec f z (a:Vector) =
           let mA = a.NumRows
           let mutable acc = z
           for i = 0 to mA-1 do acc <- f i acc a.[i]
           acc
       
       let rec forallR f (n,m) = (n > m) || (f n && forallR f (n+1,m))
       let rec existsR f (n,m) = (n <= m) && (f n || existsR f (n+1,m))
       
       let foralliDenseMatrix pred (a:DenseMatrix) =
           let nA = a.NumCols 
           let mA = a.NumRows
           let arrA = a.Values 
           (0,mA-1) |> forallR  (fun i ->
           (0,nA-1) |> forallR  (fun j ->
           pred i j (getArray2D arrA i j)))

       let foralliVec pred (a:Vector) =
           let mA = a.NumRows
           (0,mA-1) |> forallR  (fun i ->
           pred i a.[i])

       let existsiDenseMatrix pred (a:DenseMatrix) =
           let nA = a.NumCols 
           let mA = a.NumRows
           let arrA = a.Values 
           (0,mA-1) |> existsR (fun i ->
           (0,nA-1) |> existsR (fun j ->
           pred i j (getArray2D arrA i j)))

       let existsiVector pred (a:Vector) =
           let mA = a.NumRows
           (0,mA-1) |> existsR (fun i ->
           pred i a.[i])

       let sumSparseMatrix  (a:SparseMatrix) = 
           a |> nonZeroEntriesSparseMatrix |> Seq.fold (fun acc (_,_,aij) -> acc + aij) 0.0

       let inline fold2DenseMatrix f z (a:DenseMatrix) (b:DenseMatrix) =
           let nA = a.NumCols 
           let mA = a.NumRows
           let nB = b.NumCols 
           let mB = b.NumRows
           if nA <> nB || mA <> mB then invalidArg "b" "the two matrices do not have compatible dimensions"
           let arrA = a.Values 
           let arrB = b.Values 
           let mutable acc = z
           for i = 0 to mA-1 do
               for j = 0 to nA-1 do 
                  acc <- f acc (getArray2D arrA i j) (getArray2D arrB i j)
           acc

       let inline fold2Vec f z (a:Vector) (b:Vector) =
           let mA = a.NumRows
           let mB = b.NumRows
           if  mA <> mB then invalidArg "b" "the two vectors do not have compatible dimensions"
           let mutable acc = z
           for i = 0 to mA-1 do acc <- f acc a.[i] b.[i]
           acc

       let wrapList (pre,mid,post,trim) show l = 
           let post = if trim then "; ..." + post else post
           match l with 
           | []    -> [pre;post]
           | [x]   -> [pre;show x;post]
           | x::xs -> [pre;show x] @ (List.collect (fun x -> [mid;show x]) xs) @ [post]

       let showItem (x:float) = 
           try 
             x.ToString("g10",System.Globalization.CultureInfo.InvariantCulture) 
           with :? System.NotSupportedException -> (box x).ToString()
       
       let mapR f (n,m) = if m < n then [] else List.init (m-n+1) (fun i -> f (n+i))

       let primShowDenseMatrix (sepX,sepR) (a : DenseMatrix) =
           let nA = min a.NumCols DenseMaxDisplay
           let mA = min a.NumRows DenseMaxDisplay
           let showLine i = wrapList ("[",";","]", a.NumCols > nA) showItem ((0,nA-1) |> mapR  (fun j -> a.[i,j])) |> Array.ofList |> System.String.Concat
           wrapList (string nA + " " + string mA + "matrix [",";"+sepX,"]"+sepR, a.NumRows > mA) showLine [0..mA-1] |> Array.ofList |> System.String.Concat

       let showDenseMatrix     m = primShowDenseMatrix ("\n","\n") m
       let debugShowDenseMatrix m = primShowDenseMatrix (""  ,""  ) m
       
       let showVec s (a : Vector) =
           let mA = min a.NumRows VectorMaxDisplay
           wrapList (s+" [",";","]",a.NumRows > mA) showItem ((0,mA-1) |> mapR  (fun i -> a.[i])) |> Array.ofList |> System.String.Concat 

       let zero = 0.0
       let one  = 1.0
       let inline sub (x:float) (y:float) = x - y
       let inline add (x:float) (y:float) = x + y
       let inline mul (x:float) (y:float) = x * y
       let inline div (x:float) (y:float) = x / y
       let inline neg (x:float) = -x

       let constDenseMatrix  m n x      = createDenseMatrix  m n (fun _ _ -> x)
       let constVec  m x                = createVec  m   (fun _ -> x)
       let scalarDenseMatrix   x        = constDenseMatrix  1 1 x 
       let scalarVec   x                = constVec  1   x 

       // Beware - when compiled with non-generic code createArray2D creates an array of null values,
       // not zero values. Hence the optimized version can only be used when compiling with generics.
       let inline zeroDenseMatrix m n = 
           let arr = createArray2D m n 
           mkDenseMatrix arr

       // Specialized: these inline down to the efficient loops we need
       let addDenseMatrix     a b = binaryOpDenseMatrix  add a b
       let addSparse     a b = binaryOpSparseMatrix  add a b
       let addV     a b = binaryOpVec  add a b
       let subDenseMatrix     a b = binaryOpDenseMatrix  sub a b 
       let subSparse     a b = binaryOpSparseMatrix  sub a b 
       let mulSparse     a b = genericMulSparse zero add mul a b
       let subV     a b = binaryOpVec  sub a b 
       let cptMulDenseMatrix  a b = binaryOpDenseMatrix  mul a b
       let cptMulSparse  a b = binaryOpSparseMatrix  mul a b
       let cptDivDenseMatrix  a b = binaryOpDenseMatrix  div a b
       let cptDivSparse  a b = binaryOpSparseMatrix  div a b
       let cptMulV  a b = binaryOpVec  mul a b

       let cptMaxDenseMatrix  (a:dmatrix) (b:dmatrix) = binaryOpDenseMatrix  max a b
       let cptMinDenseMatrix  (a:dmatrix) (b:dmatrix) = binaryOpDenseMatrix  min a b
       let cptMaxSparse  (a:smatrix) (b:smatrix) = binaryOpSparseMatrix  max a b
       let cptMinSparse  (a:smatrix) (b:smatrix) = binaryOpSparseMatrix  min a b
       let cptMaxVec  (a:vector) (b:vector) = binaryOpVec  max a b
       let cptMinV  (a:vector) (b:vector) = binaryOpVec  min a b

       // Don't make any mistake about these ones re. performance.
       let mulDenseMatrix (a:dmatrix) (b:dmatrix) =
           let nA = a.NumCols 
           let mA = a.NumRows
           let nB = b.NumCols 
           let mB = b.NumRows
           if nA<>mB then invalidArg "b" "the two matrices do not have compatible dimensions"
           let arr = createArray2D mA nB 
           let arrA = a.Values 
           let arrB = b.Values 
           for i = 0 to mA - 1 do 
               for j = 0 to nB - 1 do 
                   let mutable r = 0.0 
                   for k = 0 to mB - 1 do 
                       r <- r + mul (getArray2D arrA i k) (getArray2D arrB k j)
                   setArray2D arr i j r
           mkDenseMatrix arr

       let mulDenseMatrixVec (a:dmatrix) (b:vector) =
           let nA = a.NumCols 
           let mA = a.NumRows
           let mB = b.NumRows 
           if nA<>mB then invalidArg "b" "the two inputs do not have compatible dimensions"
           let arr = Array.zeroCreate mA 
           let arrA = a.Values 
           let arrB = b.Values 
           for i = 0 to mA - 1 do 
               let mutable r = 0.0 
               for k = 0 to nA - 1 do 
                   r <- r + mul (getArray2D arrA i k) arrB.[k]
               arr.[i] <- r
           mkVec arr

       let vectorDenseMatrix (x:vector) = createDenseMatrix x.NumRows  1         (fun i _ -> x.[i]) 

       let scaleDenseMatrix   k m = unaryOpDenseMatrix  (fun x -> mul k x) m
       let scaleSparse   k m = unaryOpSparse  (fun x -> mul k x) m
       let scaleVec   k m = unaryOpVector  (fun x -> mul k x) m
       
       let addMFDenseMatrix   k m = unaryOpDenseMatrix  (fun x -> add k x) m
       let addMFSparse   k m = unaryOpSparse  (fun x -> add k x) m
       let addMFVec   k m = unaryOpVector  (fun x -> add k x) m

       let negDenseMatrix     m   = unaryOpDenseMatrix  (fun x -> neg x) m
       let negSparse     m   = unaryOpSparse  (fun x -> neg x) m
       let negVec     m   = unaryOpVector  (fun x -> neg x) m

       let traceDenseMatrix (a:dmatrix) =
           let nA = a.NumCols 
           let mA = a.NumRows
           if nA<>mA then invalidArg "a" "expected a square matrix";
           let arrA = a.Values 
           (0,nA-1) |> sumfR (fun i -> getArray2D arrA i i) 

       let sumDenseMatrix  a = foldDenseMatrix add zero a
       let sumVec   a = foldVec  add zero a
       let prodDenseMatrix a = foldDenseMatrix mul one  a
       let prodVec  a = foldVec  mul one  a

       let dotDenseMatrix a b = fold2DenseMatrix (fun z va vb -> add z (mul va vb)) zero a b
       let dotVec a b = fold2Vec (fun z va vb -> add z (mul va vb)) zero a b
       let sumfDenseMatrix  f m = foldDenseMatrix (fun acc aij -> add acc (f aij)) zero m
       let normDenseMatrix m = sqrt (sumfDenseMatrix (fun x -> x*x) m)

       let inplaceAddDenseMatrix  a (b:DenseMatrix) = let arrB = b.Values  in inplace_mapiDenseMatrix  (fun i j x -> x + getArray2D arrB i j) a
       let inplaceAddV    a (b:Vector) = let arrB = b.Values  in inplace_mapiVec  (fun i x   -> x + arrB.[i]) a
       let inplaceSubDenseMatrix  a (b:DenseMatrix) = let arrB = b.Values  in inplace_mapiDenseMatrix  (fun i j x -> x - getArray2D  arrB i j) a
       let inplaceSubV  a (b:Vector) = let arrB = b.Values  in inplace_mapiVec  (fun i x   -> x - arrB.[i]) a
       let inplaceCptMulDenseMatrix  a (b:DenseMatrix) = let arrB = b.Values  in inplace_mapiDenseMatrix  (fun i j x -> x * getArray2D arrB i j) a
       let inplaceCptMulV  a (b:Vector) = let arrB = b.Values  in inplace_mapiVec  (fun i x   -> x * arrB.[i]) a
       let inplaceScaleDenseMatrix  (a:float) b = inplace_mapiDenseMatrix  (fun _ _ x -> a * x) b
       let inplaceScaleV  (a:float) b = inplace_mapiVec  (fun _ x   -> a * x) b

         
       let inline dense x = DenseRepr(x)
       let inline sparse x = SparseRepr(x)
       let inline createMx  rows columns f = createDenseMatrix rows columns f |> dense
       let inline createVx  m f   = createVec m f

       let nonZeroEntriesMatrix a   = 
           match a with 
           | DenseRepr a -> nonzeroEntriesDenseMatrix a 
           | SparseRepr a -> nonZeroEntriesSparseMatrix a 

       /// Merge two sorted sequences
       let mergeSorted cf (s1: seq<_>) (s2: seq<'b>) =
           seq { use e1 = s1.GetEnumerator()
                 use e2 = s2.GetEnumerator()
                 let havee1 = ref (e1.MoveNext())
                 let havee2 = ref (e2.MoveNext())
                 while !havee1 || !havee2 do
                   if !havee1 && !havee2 then
                       let v1 = e1.Current
                       let v2 = e2.Current
                       let c = cf v1 v2 
                       if c < 0 then 
                           do havee1 := e1.MoveNext()
                           yield Some(v1),None
                       elif c = 0 then
                           do havee1 := e1.MoveNext()
                           do havee2 := e2.MoveNext()
                           yield Some(v1),Some(v2)
                       else 
                           do havee2 := e2.MoveNext()
                           yield (None,Some(v2))
                   elif !havee1 then 
                       let v1 = e1.Current
                       do havee1 := e1.MoveNext()
                       yield (Some(v1),None)
                   else 
                       let v2 = e2.Current
                       do havee2 := e2.MoveNext()
                       yield (None,Some(v2)) }

       /// Non-zero entries from two sequences
       let mergedNonZeroEntriesMatrix  (a:Matrix) (b:Matrix) = 
           let zero = 0.0
           mergeSorted (fun (i1,j1,_) (i2,j2,_) -> let c = compare i1 i2 in if c <> 0 then c else compare j1 j2) (nonZeroEntriesMatrix a) (nonZeroEntriesMatrix b)
           |> Seq.map (function | Some(i,j,v1),Some(_,_,v2) -> (v1,v2)
                                | Some(i,j,v1),None         -> (v1,zero)
                                | None,        Some(i,j,v2) -> (zero,v2)
                                | None,        None          -> failwith "unreachable")


       let arrayMatrix    xss : Matrix    = mkDenseMatrix  (Array2D.copy xss) |> dense
       let arrayVector    xss : Vector    = mkVec          (Array.copy xss)

       let initMatrix  m n f : Matrix    = createDenseMatrix m n f |> dense

       let zeroMatrix m n = 
           let arr = createArray2D m n
           mkDenseMatrix arr |> dense

       let zeroVector m  : Vector = 
           mkVec (createArray m)

       let diagnMatrix (v:Vector) n = 
           let zero = 0.0 
           let nV = v.NumRows + (if n < 0 then -n else n) 
           createMx nV nV (fun i j -> if i+n=j then v.[i] else zero)

       let diagMatrix v = diagnMatrix v 0

       let constDiagMatrix  n x : Matrix = 
           let zero = 0.0 
           createMx n n (fun i j -> if i=j then x else zero) 

       // Note: we drop sparseness on pointwise multiplication of sparse and dense.
       let inline binaryOpMatrix opDense opSparse a b = 
           match a,b with 
           | DenseRepr a,DenseRepr b -> opDense   a b |> dense
           | SparseRepr a,SparseRepr b -> opSparse a b |> sparse
           | SparseRepr a, DenseRepr b     -> opDense (toDenseSparseMatrix a) b         |> dense
           | DenseRepr  a, SparseRepr b    -> opDense a (toDenseSparseMatrix b)         |> dense

       let inline unaryOpMatrix opDense opSparse b = 
           match b with 
           | DenseRepr b -> opDense b |> dense
           | SparseRepr b -> opSparse b |> sparse

       let inline floatUnaryOpMatrix opDense opSparse b = 
           match b with 
           | DenseRepr b -> opDense b 
           | SparseRepr b -> opSparse b 

       let addMatrix a b = binaryOpMatrix addDenseMatrix addSparse a b
       let subMatrix a b = binaryOpMatrix subDenseMatrix subSparse a b
       let mulMatrix a b = binaryOpMatrix mulDenseMatrix mulSparse a b
       let cptMulMatrix a b = binaryOpMatrix cptMulDenseMatrix cptMulSparse a b
       let cptDivMatrix a b = binaryOpMatrix cptDivDenseMatrix cptDivSparse a b
       let cptMaxMatrix a b = binaryOpMatrix cptMaxDenseMatrix cptMaxSparse a b
       let cptMinMatrix a b = binaryOpMatrix cptMinDenseMatrix cptMinSparse a b

       let mulMatrixByVector a b = 
           match a with 
           | DenseRepr a -> mulDenseMatrixVec   a b 
           | SparseRepr a -> mulSparseVec a b 

       let scaleMatrix a b = unaryOpMatrix (fun b -> scaleDenseMatrix a b) (fun b -> scaleSparse a b) b

       let addFloatAndMatrix a b = unaryOpMatrix (fun b -> addMFDenseMatrix a b) (fun b -> addMFSparse a b) b

       let dotMatrix a b = 
           match a,b with 
           | DenseRepr a,DenseRepr b -> dotDenseMatrix   a b 
           | _ ->  mergedNonZeroEntriesMatrix a b |> Seq.fold (fun z (va,vb) -> z + (va * vb)) 0.0 

       let negateMatrix a = unaryOpMatrix negDenseMatrix negSparse a

       let traceSparseMatrix (a:SparseMatrix) =
           let nA = a.NumCols  
           let mA = a.NumRows 
           if nA<>mA then invalidArg "a" "expected a square matrix";
           (0,nA-1) |> sumR (fun i -> a.[i,i]) 

       let prodSparseMatrix  (a:SparseMatrix) = a |> toDenseSparseMatrix |> prodDenseMatrix
       let normSparseMatrix (a:SparseMatrix) = 
           a |> nonZeroEntriesSparseMatrix |> Seq.fold (fun acc (_,_,aij) -> acc + aij*aij) 0.0 |> sqrt


       let traceMatrix a = floatUnaryOpMatrix traceDenseMatrix traceSparseMatrix a
       let sumMatrix a = floatUnaryOpMatrix sumDenseMatrix sumSparseMatrix a
       let prodMatrix a = floatUnaryOpMatrix prodDenseMatrix prodSparseMatrix a
       let normMatrix a = floatUnaryOpMatrix normDenseMatrix normSparseMatrix a

       let transposeMatrix a = 
           match a with 
           | DenseRepr a -> 
               // rows of transposed matrix = columns of original matrix and vice versa
               createMx a.NumCols a.NumRows (fun i j -> a.[j,i])
           | SparseRepr a -> 
               a |> nonZeroEntriesSparseMatrix  |> Seq.map (fun (i,j,v) -> (j,i,v)) |> initSparseMatrix a.NumCols a.NumRows |> sparse
       
       let permuteRows (p: permutation) a =
           match a with
           | DenseRepr a ->
               createMx a.NumRows a.NumCols (fun i j -> a.[p i,j])
           | SparseRepr a ->
               a |> nonZeroEntriesSparseMatrix  |> Seq.map (fun (i,j,v) -> (p i,j,v)) |> initSparseMatrix a.NumCols a.NumRows |> sparse

       let permuteColumns (p: permutation) a =
           match a with
           | DenseRepr a ->
               createMx a.NumRows a.NumCols (fun i j -> a.[i,p j])
           | SparseRepr a ->
               a |> nonZeroEntriesSparseMatrix  |> Seq.map (fun (i,j,v) -> (i,p j,v)) |> initSparseMatrix a.NumCols a.NumRows |> sparse

       let inplaceAddMatrix a b = 
           match a,b with 
           | DenseRepr a,DenseRepr b -> inplaceAddDenseMatrix   a b
           | _ -> sparseNotMutable()

       let inplaceSubMatrix a b = 
           match a,b with 
           | DenseRepr a,DenseRepr b -> inplaceSubDenseMatrix   a b
           | _ -> sparseNotMutable()

       let inplaceCptMulMatrix a b = 
           match a,b with 
           | DenseRepr a,DenseRepr b -> inplaceCptMulDenseMatrix   a b
           | _ -> sparseNotMutable()

       let inplaceScaleMatrix a b = 
           match b with 
           | DenseRepr b -> inplaceScaleDenseMatrix   a b
           | _ -> sparseNotMutable()

       let existsMatrix  f a = 
           match a with 
           | SparseRepr _ -> sparseNYI() // note: martin says "run f on a token element if it's not full"
           | DenseRepr a -> existsiDenseMatrix  (fun _ _ -> f) a

       let existsVector  f a = existsiVector  (fun _ -> f) a

       let forallMatrix  f a = 
           match a with 
           | SparseRepr _ -> sparseNYI()
           | DenseRepr a -> foralliDenseMatrix  (fun _ _ -> f) a

       let forallVector  f a = foralliVec  (fun _ -> f) a

       let existsiMatrix  f a = 
           match a with 
           | SparseRepr _ -> sparseNYI()
           | DenseRepr a -> existsiDenseMatrix  f a

       let foralliMatrix  f a = 
           match a with 
           | SparseRepr _ -> sparseNYI()
           | DenseRepr a -> foralliDenseMatrix  f a

       let foralliVector  f a = foralliVec  f a

       let mapMatrix  f a = 
           match a with 
           | SparseRepr _ -> sparseNYI()
           | DenseRepr a -> DenseRepr(mapDenseMatrix f a)

       let copyMatrix  a = 
           match a with 
           | SparseRepr a -> SparseRepr (copySparse a)
           | DenseRepr a -> DenseRepr (copyDenseMatrix a)

       let mapiMatrix  f a = 
           match a with 
           | SparseRepr _ -> sparseNYI()
           | DenseRepr a -> DenseRepr (mapiDenseMatrix f a)

       let toDenseMatrix a = 
           match a with 
           | SparseRepr a -> toDenseSparseMatrix a |> dense
           | DenseRepr _ -> a

       let initSparseM i j x : Matrix = 
           initSparseMatrix i j x |> sparse
         
       let initDenseMatrix i j x : Matrix = 
           let r = zeroMatrix i j
           x |> Seq.iter (fun (i,j,v) -> r.[i,j] <- v);
           r

       let getDiagnMatrix (a:Matrix) n =
           let nA = a.NumCols 
           let mA = a.NumRows
           if nA<>mA then invalidArg "a" "expected a square matrix";
           let ni = if n < 0 then -n else 0 
           let nj = if n > 0 then  n else 0 
           createVec (max (nA-abs(n)) 0) (fun i -> a.[i+ni,i+nj]) 

       let getDiagMatrix  a = getDiagnMatrix a 0

       let inline foldMatrix  f z a = 
           match a with 
           | SparseRepr _ -> sparseNYI()
           | DenseRepr a -> foldDenseMatrix f z a

       let inline foldVector  f z a = foldVec f z a

       let inline foldiMatrix  f z a = 
           match a with 
           | SparseRepr _ -> sparseNYI()
           | DenseRepr a -> foldiDenseMatrix f z a

       let inline foldiVector  f z a = foldiVec f z a

       let compareMatrix (comp: IComparer) (a:Matrix) (b:Matrix) = 
           let nA = a.NumCols 
           let mA = a.NumRows 
           let nB = b.NumCols 
           let mB = b.NumRows 
           let c = compare mA mB 
           if c <> 0 then c else
           let c = compare nA nB 
           if c <> 0 then c else
           match a,b with 
           | DenseRepr a, DenseRepr b -> 
             let rec go2 i j = 
                if j < nA then 
                  let c = comp.Compare( a.[i,j], b.[i,j])
                  if c <> 0 then c else 
                  go2 i (j+1) 
                else 0 
             let rec go1 i = 
                if i < mA then 
                  let c = go2 i 0 
                  if c <> 0 then c 
                  else go1 (i+1) 
                else 0 
             go1 0
           | _ -> 
             match (mergedNonZeroEntriesMatrix a b |> Seq.tryPick (fun (v1,v2) -> let c = comp.Compare(v1,v2) in if c = 0 then None else Some(c))) with
             | None -> 0
             | Some(c) -> c
            
       let equalsMatrix (comp: IEqualityComparer) (a:Matrix) (b:Matrix) = 
           let nA = a.NumCols 
           let mA = a.NumRows 
           let nB = b.NumCols 
           let mB = b.NumRows 
           (mA = mB ) && (nA = nB) && 
           match a,b with 
           | DenseRepr a, DenseRepr b -> 
               let rec go2 i j =  j >= nA || (comp.Equals( a.[i,j], b.[i,j]) && go2 i (j+1) )
               let rec go1 i = i >= mA || (go2 i 0  && go1 (i+1))
               go1 0
           | _ -> 
               mergedNonZeroEntriesMatrix a b |> Seq.forall (fun (v1,v2) -> comp.Equals(v1,v2))
            

       let compareVector (comp: IComparer) (a:Vector) (b:Vector) = 
           let mA = a.NumRows
           let mB = b.NumRows 
           let c = compare mA mB 
           if c <> 0 then c else
           let rec go2 j = 
              if j < mA then 
                let c = comp.Compare(a.[j],b.[j])
                if c <> 0 then c else go2 (j+1) 
              else 0 
           go2 0

       let equalsVector (comp: IEqualityComparer) (a:Vector) (b:Vector) = 
           let mA = a.NumRows
           let mB = b.NumRows 
           (mA = mB) && 
           let rec go2 j = (j >= mA) || (comp.Equals(a.[j],b.[j]) && go2 (j+1))
           go2 0

       let inline combineHash x y = (x <<< 1) + y + 631 

       let hashMatrix (comp:IEqualityComparer) (a:Matrix) = 
           let nA = a.NumCols 
           let mA = a.NumRows 
           let acc = hash mA + hash nA
           a |> nonZeroEntriesMatrix |> Seq.truncate 20 |> Seq.fold (fun z v -> combineHash z (comp.GetHashCode v)) acc
         
       let hashVector (comp:IEqualityComparer) (a:Vector) = 
           let mA = a.NumRows 
           hash mA +
           (let mutable c = 0 
            for i = 0 to mA - 1 do
                c <- combineHash c (comp.GetHashCode a.[i])
            c)
         
       type range = int * int

       let startR ((a,_) : range)   = a
       let countR ((a,b) : range)   = (b-a)+1
       let idxR    ((a,_) : range) i = a+i
       let inR    ((a,b) : range) i = a <= i && i <= b
       
       let getRowMatrix  (a:Matrix) i = createVx a.NumCols (fun j -> a.[i,j])
       let getColMatrix  (a:Matrix) j = createVx a.NumRows (fun i -> a.[i,j]) 
       let getRegionVector  (a:Vector)    r      = createVx (countR r) (fun i -> a.[idxR r i]) 

       let getRegionMatrix  a ri rj    = 
           match a with 
           | DenseRepr a -> createMx (countR ri) (countR rj) (fun i j -> a.[idxR ri i, idxR rj j]) 
           | _ -> nonZeroEntriesMatrix a 
                  |> Seq.filter (fun (i,j,_) -> inR ri i && inR rj j) 
                  |> Seq.map (fun (i,j,v) -> (i-startR ri,j-startR rj,v)) 
                  |> initSparseM (countR ri) (countR rj)

       let getColsMatrix (a:Matrix) rj         = getRegionMatrix a (0,a.NumRows - 1) rj //todo
       let getRowsMatrix (a:Matrix) ri         = getRegionMatrix a ri (0,a.NumCols - 1) //todo

   open Impl


//----------------------------------------------------------------------------
// type Matrix augmentation
//--------------------------------------------------------------------------*)

   type Matrix with
       static member ( +  )(a: Matrix,b) = addMatrix a b
       static member ( -  )(a: Matrix,b) = subMatrix a b
       static member ( *  )(a: Matrix,b) = mulMatrix a b
       static member ( *  )(a: Matrix,b : Vector) = mulMatrixByVector a b

       static member ( * )((m: Matrix),k : float) = scaleMatrix k m
       static member ( / )((m: Matrix),k : float) = scaleMatrix (1.0/k) m

       static member ( + )(m: Matrix,k : float) = addFloatAndMatrix k m
       static member ( - )(m: Matrix,k : float) = m + (-k)

       static member ( .* )(a: Matrix,b) = cptMulMatrix a b
       static member ( ./ )(a: Matrix,b) = cptDivMatrix a b
       static member ( * )(k,m: Matrix) = scaleMatrix k m
       static member ( ~- )(m: Matrix)     = negateMatrix m
       static member ( ~+ )(m: Matrix)     = m

       member m.GetSlice (start1,finish1,start2,finish2) = 
           let start1 = match start1 with None -> 0 | Some v -> v 
           let finish1 = match finish1 with None -> m.NumRows - 1 | Some v -> v 
           let start2 = match start2 with None -> 0 | Some v -> v 
           let finish2 = match finish2 with None -> m.NumCols - 1 | Some v -> v 
           getRegionMatrix m (start1,finish1) (start2,finish2)

       member m.SetSlice (start1,finish1,start2,finish2,vs:Matrix) = 
           let start1 = match start1 with None -> 0 | Some v -> v 
           let finish1 = match finish1 with None -> m.NumRows - 1 | Some v -> v 
           let start2 = match start2 with None -> 0 | Some v -> v 
           let finish2 = match finish2 with None -> m.NumCols - 1 | Some v -> v 
           for i = start1 to finish1  do 
              for j = start2 to finish2 do
                  m.[i,j] <- vs.[i-start1,j-start2]

       override m.ToString() = 
          match m with 
          | DenseRepr m -> Impl.showDenseMatrix m
          | SparseRepr _ -> "<sparse>"

       member m.DebugDisplay = 
          let txt = 
              match m with 
              | DenseRepr m -> Impl.debugShowDenseMatrix m
              | SparseRepr _ -> "<sparse>"
          new System.Text.StringBuilder(txt)  // return an object with a ToString with the right value, rather than a string. (strings get shown using quotes)

       member m.StructuredDisplayAsArray =  
           let rec layout m = 
               match m with 
               | DenseRepr _ -> box (Array2D.init m.NumRows m.NumCols (fun i j -> m.[i,j]))
               | SparseRepr _ -> (if m.NumRows < 20 && m.NumCols < 20 then layout (toDenseMatrix m) else box(nonZeroEntriesMatrix m))
           layout m
       member m.Dimensions = m.NumRows,m.NumCols

       member m.Transpose() = transposeMatrix m
       member m.PermuteRows (p: permutation) : Matrix = permuteRows p m
       member m.PermuteColumns (p: permutation) : Matrix = permuteColumns p m

       interface System.IComparable with 
            member m.CompareTo(yobj:obj) = compareMatrix LanguagePrimitives.GenericComparer m (yobj :?> Matrix)
            
       interface IStructuralComparable with
           member m.CompareTo(yobj:obj,comp:System.Collections.IComparer) = compareMatrix comp m (yobj :?> Matrix)
           
       override m.GetHashCode() = hashMatrix LanguagePrimitives.GenericEqualityComparer m 
       override m.Equals(yobj:obj) = 
           match yobj with 
           | :? Matrix as m2 -> equalsMatrix LanguagePrimitives.GenericEqualityComparer m m2
           | _ -> false
       
       interface IStructuralEquatable with
           member m.GetHashCode(comp:System.Collections.IEqualityComparer) = hashMatrix comp m
           member m.Equals(yobj:obj,comp:System.Collections.IEqualityComparer) = 
               match yobj with 
               | :? Matrix as m2 -> equalsMatrix comp m m2
               | _ -> false


//----------------------------------------------------------------------------
// type Vector augmentation
//--------------------------------------------------------------------------*)

   type Vector with
       static member ( +  )(a: Vector,b) = addV a b
       static member ( -  )(a: Vector,b) = subV a b
       static member ( .* )(a: Vector,b) = cptMulV a b
       
       static member ( * )(k,m: Vector) = scaleVec k m
       
       static member ( * )(m: Vector,k) = scaleVec k m
       
       static member ( ~- )(m: Vector)     = negVec m
       static member ( ~+ )(m: Vector)     = m

       member m.GetSlice (start,finish) = 
           let start = match start with None -> 0 | Some v -> v 
           let finish = match finish with None -> m.NumRows - 1 | Some v -> v 
           getRegionVector m (start,finish)

       member m.SetSlice (start,finish,vs:Vector) = 
           let start = match start with None -> 0 | Some v -> v 
           let finish = match finish with None -> m.NumRows - 1 | Some v -> v 
           for i = start to finish  do 
                  m.[i] <- vs.[i-start]


       override m.ToString() = Impl.showVec "vector" m

       member m.DebugDisplay = 
           let txt = Impl.showVec "vector" m
           new System.Text.StringBuilder(txt)  // return an object with a ToString with the right value, rather than a string. (strings get shown using quotes)

       member m.StructuredDisplayAsArray =  Array.init m.NumRows (fun i -> m.[i])

       member m.Details = m.Values

       member m.Permute (p:permutation) = permuteV p m
     
       interface System.IComparable with 
            member m.CompareTo(y:obj) = compareVector LanguagePrimitives.GenericComparer m (y :?> Vector)
       
       interface IStructuralComparable with
           member m.CompareTo(y:obj,comp:System.Collections.IComparer) = compareVector comp m (y :?> Vector)

       interface IStructuralEquatable with
           member x.GetHashCode(comp) = hashVector comp x
           member x.Equals(yobj,comp) = 
               match yobj with 
               | :? Vector as v2 -> equalsVector comp x v2
               | _ -> false

       override x.GetHashCode() = 
           hashVector LanguagePrimitives.GenericEqualityComparer x

       override x.Equals(yobj) = 
           match yobj with 
           | :? Vector as v2 -> equalsVector LanguagePrimitives.GenericEqualityComparer x v2
           | _ -> false


   type matrix = Matrix
   type vector = Vector

   module MRandom = 
       let seed = 99
       let randomGen = new System.Random(seed)
       let float f = randomGen.NextDouble() * f 


   [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
   module Matrix = begin
       
       let inline dense x = DenseRepr(x)
       let inline sparse x = SparseRepr(x)

       // Element type OpsData
       type elem = float

       // Accessors
       let get (a:matrix) (i,j)   = a.[i,j]
       let set (a:matrix) (i,j) x = a.[i,j] <- x
       
       // Creation
       let init  (m,n) f = createDenseMatrix  m n f |> dense 
       let ofList    xss   = listDenseMatrix    xss |> dense
       let ofSeq     xss   = seqDenseMatrix    xss |> dense
       let ofColumns xss   = ofSeq xss
       let ofRows    xss   = ofSeq(xss).Transpose()
       let diag  (v:vector)   = diagMatrix v 
       let initDiagonal  (v:vector)   = diagMatrix v 
       let constDiag  n x : matrix  = constDiagMatrix n x 
       let create  (m,n) x  = constDenseMatrix  m n x |> dense
       let ofScalar x     = scalarDenseMatrix x |> dense

       let ofArray2D arr : matrix = arrayMatrix arr
       let toArray2D (m : matrix) = Array2D.init m.NumRows m.NumCols (fun i j -> get m (i,j))

       let getDiagN  (a:matrix) n = getDiagnMatrix a n
       let getDiag  (a:matrix) = getDiagMatrix a

       // Operators
       let add (a:matrix) (b:matrix) = addMatrix   a b
       let sub (a:matrix) (b:matrix) = subMatrix   a b
       let mul (a:matrix) (b:matrix) = mulMatrix   a b
       let mulV (a:matrix) (b:vector) = mulMatrixByVector   a b
       let cptMul (a:matrix) (b:matrix) = cptMulMatrix   a b
       let cptMax (a:matrix) (b:matrix) = cptMaxMatrix a b
       let cptMin (a:matrix) (b:matrix) = cptMinMatrix a b
       let scale a (b:matrix) = scaleMatrix   a b
       let neg (a:matrix)  = negateMatrix a
       let trace (a:matrix)  = traceMatrix a
       let transpose  (a:matrix) = transposeMatrix a
       let forall f (a:matrix) = forallMatrix f a
       let exists  f (a:matrix) = existsMatrix f a
       let foralli f (a:matrix) = foralliMatrix f a
       let existsi  f (a:matrix) = existsiMatrix f a
       let map  f (a:matrix) = mapMatrix f a
       let copy  (a:matrix) = copyMatrix a
       let mapi  f (a:matrix) : matrix = mapiMatrix f a
       let fold  f z (a:matrix) = foldMatrix f z a
       let foldi  f z (a:matrix) = foldiMatrix f z a

       let toDense (a:matrix) = toDenseMatrix a 
       let initDense (i,j) a : matrix = initDenseMatrix i j a 
       let initSparse (i,j) a : matrix = initSparseM i j a 

       let zero (m,n)  = zeroDenseMatrix m n |> dense

       let identity m  : matrix = 
           let arr = createArray2D m m 
           for i = 0 to m - 1 do 
              arr.[i,i] <- 1.0 
           mkDenseMatrix arr |> dense
       
       let ones (m,n)  = create (m,n) 1.0
       
       let getRow (a:matrix) i      = getRowMatrix a i
       let getCol (a:matrix) j      = getColMatrix a j
       let getCols (a:matrix) i1 i2    = getColsMatrix a  (i1,i1+i2-1)
       let getRows (a:matrix) j1 j2    = getRowsMatrix a (j1,j1+j2-1)
       let getRegion (a:matrix) i1 j1 i2 j2    = getRegionMatrix a (i1,i1+i2-1) (j1,j1+j2-1)
       let ofRowVector (x:vector) = initMatrix 1          x.Length  (fun _ j -> x.[j]) 

       let rowRange (a:Matrix) = (0,a.NumRows - 1)
       let colRange (a:Matrix) = (0,a.NumCols - 1)
       let wholeRegion a = (colRange a, rowRange a)
       
       let foldByRow f (z:Vector) (a:matrix) = 
         colRange a |> Impl.foldR (fun z j -> mapiV (fun i z -> f z (get a (i,j))) z) z
       let foldByCol f (z:Vector) (a:matrix) = 
         rowRange a |> Impl.foldR (fun z i -> mapiV (fun j z -> f z (get a (i,j))) z) z

       let foldRow f (z:'T) (a:matrix) i = 
         colRange a |> Impl.foldR (fun (z:'T) j -> f z (get a (i,j))) z
       let foldCol f (z:'T) (a:matrix) j = 
         rowRange a |> Impl.foldR (fun (z:'T) i -> f z (get a (i,j))) z

       let sum (a:matrix)  = sumMatrix a
       let prod (a:matrix)  = prodMatrix a
       let norm  (a:matrix) = normMatrix  a
       let dot (a:matrix) b = dotMatrix a b

       let cptPow  a y = map (fun x -> x ** y) a
       
       // Functions that only make sense on this type
       let randomize v = map (fun vij -> MRandom.float vij) v      (* res_ij = random [0,vij] values *)

       let ofVector    (x:vector) : matrix = initMatrix x.NumRows  1         (fun i _ -> x.[i])  
       let toVector    x : vector = getColMatrix x 0 
       let toRowVector    x : vector = getRowMatrix x 0 
       let toScalar    (x : matrix)  = x.[0,0]

       let inplaceAdd  (a:matrix) b = inplaceAddMatrix a b
       let inplaceSub  (a:matrix) b = inplaceSubMatrix a b



   end


//----------------------------------------------------------------------------
// module Vector
//--------------------------------------------------------------------------*)
     
   [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
   module Vector = 

       let get (a:vector) j   = a.[j]
       let set (a:vector) j x = a.[j] <- x
       let length (a:vector)     = a.Length
       let nrows (a:vector)   = a.Length
       let init  m   f = createVec  m   f
       let ofArray arr : vector = arrayVector arr
       let toArray (v : vector) = Array.init v.Length (get v)

       type range = int * int
       let countR ((a,b) : range)   = (b-a)+1
       let idxR    ((a,_) : range) i = a+i
       type rangef = float * float * float // start, skip, end
       let countRF ((a,d,b) : rangef)   = System.Convert.ToInt32((b-a)/d) + 1
       //let countRF ((a,d,b) : rangef)   = Float.to_int((b-a)/d) + 1
       let idxRF  ((a,d,b) : rangef) i = System.Math.Min (a + d * float(i),b)

       let range n1 n2    = let r = (n1,n2)   in init (countR  r) (fun i -> float(idxR r i)) 

       let rangef a b c  = let r = (a,b,c) in init (countRF r) (fun i -> idxRF r i)

       let ofList    xs    = listVec    xs
       let ofSeq    xs    = seqVec    xs
       let create  m   x  = constVec  m   x
       let ofScalar x     = scalarVec x
       let add a b = addV   a b
       let sub a b = subV   a b
       let cptMul a b = cptMulV   a b
       let cptMax a b = cptMaxVec a b
       let cptMin a b = cptMinV a b
       let scale a b = scaleVec   a b
       let neg a  = negVec a
       let dot a b = dotVec a b
       let exists  f (a:vector) = existsVector f a
       let forall  f (a:vector) = forallVector f a
       let existsi  f (a:vector) = existsiVector f a
       let foralli  f (a:vector) = foralliVector f a
       let map  f (a:vector) = mapV f a
       let copy (a:vector) = copyV a
       let mapi  f (a:vector) : vector = mapiV f a
       let fold  f z (a:vector) = foldVector f z a
       let foldi  f z (a:vector) = foldiVector f z a
       let zero n = create n 0.0
       let ones n = create n 1.0
       let sum a  = sumVec a
       let prod a   = fold      (fun x y -> x * y) 1.0 a
       let norm  (a:vector) = sqrt (fold (fun x y -> x + y * y) 0.0 a) (* fixed *)
       let cptPow  a y = map  (fun x -> x ** y) a



   type Matrix with 
       member x.ToArray2D()        = Matrix.toArray2D x

#if FX_NO_DEBUG_DISPLAYS
#else
       [<DebuggerBrowsable(DebuggerBrowsableState.Collapsed)>]
#endif

       member x.NonZeroEntries    = nonZeroEntriesMatrix x
       member x.ToScalar()        = Matrix.toScalar x
       member x.ToVector()        = Matrix.toVector x

#if FX_NO_DEBUG_DISPLAYS
#else
       [<DebuggerBrowsable(DebuggerBrowsableState.Collapsed)>]
#endif
       member x.Norm              = Matrix.norm x

       member x.Column(n)         = Matrix.getCol x n
       member x.Row(n)            = Matrix.getRow x n
       member m.Columns           = IndexedList m (fun m -> m.NumCols) (fun m i -> m.Column(i))
       member m.Rows              = IndexedList m (fun m -> m.NumRows) (fun m i -> m.Row(i))
       member x.Region(i,j,ni,nj) = Matrix.getRegion x i j ni nj
       member x.GetDiagonal(i)    = Matrix.getDiagN x i

#if FX_NO_DEBUG_DISPLAYS
#else
       [<DebuggerBrowsable(DebuggerBrowsableState.Collapsed)>]
#endif
       member x.Diagonal          = Matrix.getDiag x

       member x.Copy () = Matrix.copy x


   type Vector with 
       member x.ToArray() = Vector.toArray x
       member x.Norm      = Vector.norm x
       member x.Copy ()   = Vector.copy x




   module MatrixTopLevelOperators = 

       let matrix ll = Matrix.ofSeq ll
       let vector l  = Vector.ofSeq  l
