
namespace global

module Array =
    /// Find the maximum element and its index
    let maxi (xs:'a[]) =
        match xs with
        | [|x|] -> (x,0)
        | xs ->
            let xs' = xs |> Array.mapi (fun i x -> (x,i))
            xs' |> Array.fold (fun (max,i) (x,j) -> if x > max then (x,j) else (max,i)) (xs'.[0])

    /// ???
    let reduceWith (f:'a -> 'a -> 'a) (t:'a) (xs:'a[]) =
            Array.reduce (fun x y -> f (f x t) y) xs

    /// Apply the selector to each element and combine the results using the reducer
    let mapReduce (selector:'a -> 'b) (reducer:'b -> 'b -> 'b) (xs:'a[]) =
        xs |> Array.map selector
           |> Array.reduce reducer

    /// ???
    let mapReduceWith (map:'a -> 'b) (reduce:'b -> 'b -> 'b) (t:'b) (xs:'a[]) =
            xs |> Array.map map
               |> reduceWith reduce t

    /// Like Seq.groupBy, but returns arrays 
    let classifyBy f (xs:_[]) = xs |> Seq.groupBy f |> Seq.map (fun (k,v) -> (k,Seq.toArray v)) |> Seq.toArray
