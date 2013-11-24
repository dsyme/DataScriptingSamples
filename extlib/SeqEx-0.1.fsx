
namespace global

module Seq = 

    /// Return x, f(x), f(f(x)), f(f(f(x))), ...
    let iterate f x =  Seq.unfold (fun acc -> let x = f acc in Some (x,x)) x

    /// Add integer indexes to the elements of the sequence
    let index source = source |> Seq.mapi (fun i x -> (i,x))       

    /// Count the number of elements that satisfy the given predicate
    let lengthBy predicate source = source |> Seq.filter predicate |> Seq.length

    /// Produce an IDictionary for the values in the sequence
    let indexBy key source = source |> Seq.map (fun x -> key x, x) |> dict
