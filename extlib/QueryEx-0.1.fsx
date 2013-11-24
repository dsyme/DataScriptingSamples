namespace global

[<AutoOpen>]
module QueryStuff = 
    open System
    open System.Linq
    open System.Linq.Expressions

    /// Provides F#-like operators that preserve the remote-execution characteristics of LINQ IQueryable objects
    type Query() = 
        static member filter (q:Expression<Func<_,_>>) = (fun (v:IQueryable<_>) -> v.Where(q))
        static member filteri (q:Expression<Func<_,_,_>>) = (fun (v:IQueryable<_>) -> v.Where(q))
        static member where (q:Expression<Func<_,_>>) = (fun (v:IQueryable<_>) -> v.Where(q))
        static member wherei (q:Expression<Func<_,_,_>>) = (fun (v:IQueryable<_>) -> v.Where(q))
        static member map (q:Expression<Func<_,_>>) = (fun (v:IQueryable<_>) -> v.Select(q))
        static member mapi (q:Expression<Func<_,_,_>>) = (fun (v:IQueryable<_>) -> v.Select(q))
        static member averageBy (q:Expression<Func<'T,float<'u>>>) = (fun (v:IQueryable<_>) -> v.Average(q :> obj :?> Expression<Func<'T,float>>) |> box |> unbox : float<'u>)
        static member averageByNullable (q:Expression<Func<_,Nullable<float<'u>>>>) = (fun (v:IQueryable<_>) -> v.Average(q :> obj :?> Expression<Func<'T,Nullable<float>>>) |> box |> unbox : Nullable<float<'u>>)
        static member sumBy (q:Expression<Func<_,float<'u>>>) = (fun (v:IQueryable<_>) -> v.Sum(q :> obj :?> Expression<Func<'T,float>>) |> box |> unbox : float<'u> )
        static member sumByNullable (q:Expression<Func<_,Nullable<float<'u>>>>) = (fun (v:IQueryable<_>) -> v.Sum(q :> obj :?> Expression<Func<'T,Nullable<float>>>) |> box |> unbox : Nullable<float<'u>> )
        static member forall (q:Expression<Func<_,bool>>) = (fun (v:IQueryable<_>) -> v.All(q))
        static member exists (q:Expression<Func<_,bool>>) = (fun (v:IQueryable<_>) -> v.Any(q))
        static member reduce (q:Expression<Func<'T,'T,'T>>) = (fun (v:IQueryable<_>) -> v.Aggregate(q))
        static member fold (q:Expression<Func<'TAccumulate,'TSource,'TAccumulate>>) seed = (fun (v:IQueryable<_>) -> v.Aggregate(seed,q))
        static member find (q:Expression<Func<_,bool>>) = (fun (v:IQueryable<_>) -> v.First(q))
        static member findOrDefault (q:Expression<Func<_,bool>>) = (fun (v:IQueryable<_>) -> v.FirstOrDefault(q))
        //static member tryFind (q:Expression<Func<_,bool>>) = (fun (v:IQueryable<_>) -> v.FirstOrDefault(q))
        [<CompilerMessage("Grouping for IQueryable data sources should be done using the full F# query comprehension syntax 'query { for x in source do groupBy selector into group ... }' followed by nested queries for extracting further results from groups.",10001,IsError=true)>]
        static member groupBy x = failwith "not allowed"
        [<CompilerMessage("Joins on IQueryable data sources should be done using the full F# query comprehension syntax 'query { for x in source1 do groupJoin source2 on (key1 = key2) into group ... }' followed by nested queries for extracting further results from groups.",10001,IsError=true)>]
        static member groupJoin x = failwith "not allowed"
        [<CompilerMessage("Joins on IQueryable data sources should be done using the full F# query comprehension syntax  'query { for x in source1 do join source2 on (key1 = key2) ... }' followed by further query operations.",10001,IsError=true)>]
        static member join x = failwith "not allowed" 
        static member maxBy (q:Expression<Func<_,_>>) = (fun (v:IQueryable<_>) -> v.Max(q))
        static member minBy (q:Expression<Func<_,_>>) = (fun (v:IQueryable<_>) -> v.Min(q))
        static member maxByNullable (q:Expression<Func<_,Nullable<_>>>) = (fun (v:IQueryable<_>) -> v.Max(q))
        static member minByNullable (q:Expression<Func<_,Nullable<_>>>) = (fun (v:IQueryable<_>) -> v.Min(q))
        static member sortBy (q:Expression<Func<_,'TKey>> when 'TKey : comparison) = (fun (v:IQueryable<_>) -> v.OrderBy(q))
        static member sortByDescending (q:Expression<Func<_,'TKey>> when 'TKey : comparison) = (fun (v:IQueryable<_>) -> v.OrderByDescending(q))
        static member sortByNullable (q:Expression<Func<_,Nullable<'TKey>>> when 'TKey : comparison) = (fun (v:IQueryable<_>) -> v.OrderBy(q))
        static member sortByNullableDescending (q:Expression<Func<_,Nullable<'TKey>>> when 'TKey : comparison) = (fun (v:IQueryable<_>) -> v.OrderByDescending(q))
        static member thenBy (q:Expression<Func<_,'TKey>> when 'TKey : comparison) = (fun (v:IOrderedQueryable<_>) -> v.ThenBy(q))
        static member thenByDescending (q:Expression<Func<_,'TKey>> when 'TKey : comparison) = (fun (v:IOrderedQueryable<_>) -> v.ThenByDescending(q))
        static member thenByNullable (q:Expression<Func<_,Nullable<'TKey>>> when 'TKey : comparison) = (fun (v:IOrderedQueryable<_>) -> v.ThenBy(q))
        static member thenByDescendingNullable (q:Expression<Func<_,Nullable<'TKey>>> when 'TKey : comparison) = (fun (v:IOrderedQueryable<_>) -> v.ThenByDescending(q))
        static member collect (q:Expression<Func<_,_>>) = (fun (v:IQueryable<_>) -> v.SelectMany(q))

    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module Query =     
        let contains<'T> x (source:IQueryable<'T>) = source.Contains x
        let elementAt<'T> index (source:IQueryable<'T>) = source.ElementAt(index)
        let elementAtOrDefault<'T> index (source:IQueryable<'T>) = source.ElementAtOrDefault(index)
        let count<'T> (source:IQueryable<'T>) = source.Count()
        let longCount<'T> (source:IQueryable<'T>) = source.LongCount()
        let last<'T> (source:IQueryable<'T>) = source.Last()
        let lastOrDefault<'T> (source:IQueryable<'T>) = source.LastOrDefault()
        let head<'T> (source:IQueryable<'T>) = source.First()
        let headOrDefault<'T> (source:IQueryable<'T>) = source.FirstOrDefault()
        let exactlyOne<'T> (source:IQueryable<'T>) = source.Single()
        let exactlyOneOrDefault<'T> (source:IQueryable<'T>) = source.SingleOrDefault()
        let distinct<'T> (source:IQueryable<'T>) = source.Distinct()
        let max<'T when 'T : comparison> (source:IQueryable<'T>) = source.Max()
        let iter (f:_ -> unit) (source:IQueryable<_>) = Seq.iter f source
        let toSeq (source:IQueryable<_>) = source.AsEnumerable()
        let toList (source:IQueryable<_>) = Seq.toList source
        let toArray<'T> (source:IQueryable<'T>) = Seq.toArray source
        let skip<'T> n (source:IQueryable<'T>) = source.Skip(n)
        let take<'T> n (source:IQueryable<'T>) = source.Take(n)
        // These are not portable
        //let parallelize (source:IQueryable<_>) = source.AsParallel()
        //let tryHead = (fun (source:IQueryable<'T>) -> source.Last())
        //let tryExactlyOne = (fun (source:IQueryable<'T>) -> source.Last())
        //let tryLast = (fun (source:IQueryable<'T>) -> source.Last())

