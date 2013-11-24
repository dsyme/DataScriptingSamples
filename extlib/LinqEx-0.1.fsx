namespace global

[<AutoOpen>]
module QuickLinqHelpers = 
    open System
    open System.Linq
    open System.Linq.Expressions
    open System.Collections.Generic
 
    type IQueryable<'T> with
 
        ///  A version of Queryable.OrderBy which does not require a type annotation on the argument when used from F#
 
        member x.OrderByQ(keySelector : Expression<Func<'T,'TKey>>) = System.Linq.Queryable.OrderBy(x,keySelector)
 
        ///  A version of Queryable.OrderByDescending which does not require a type annotation on the argument when used from F#
 
        member x.OrderByDescendingQ(keySelector : Expression<Func<'T,'TKey>>) = System.Linq.Queryable.OrderByDescending(x,keySelector)
 
        ///  A version of Queryable.Select which does not require a type annotation on the argument when used from F#
 
        member x.SelectQ(selector : Expression<Func<'T,'TResult>>) = System.Linq.Queryable.Select(x,selector)
 
        ///  A version of Queryable.SelectMany which does not require a type annotation on the argument when used from F#
 
        member x.SelectManyQ(selector : Expression<Func<'T,IEnumerable<'TResult>>>) = System.Linq.Queryable.SelectMany(x,selector)
 
        ///  A version of Queryable.SkipWhile which does not require a type annotation on the argument when used from F#
 
        member x.SkipWhileQ(predicate : Expression<Func<'T,bool>>) = System.Linq.Queryable.SkipWhile(x,predicate)
 
        ///  A version of Queryable.TakeWhile which does not require a type annotation on the argument when used from F#
 
        member x.TakeWhileQ(predicate : Expression<Func<'T,bool>>) = System.Linq.Queryable.TakeWhile(x,predicate)
 
        ///  A version of Queryable.Where which does not require a type annotation on the argument when used from F#
 
        member x.WhereQ(predicate : Expression<Func<'T,bool>>) = System.Linq.Queryable.Where(x,predicate)
 
 
 
 
 
    type IOrderedQueryable<'T> with
 
        ///  A version of Queryable.ThenBy which does not require a type annotation on the argument when used from F#
 
        member x.ThenByQ(keySelector : Expression<Func<'T,'TKey>>) = System.Linq.Queryable.ThenBy(x,keySelector)
 
        ///  A version of Queryable.ThenByDescending which does not require a type annotation on the argument when used from F#
 
        member x.ThenByDescendingQ(keySelector : Expression<Func<'T,'TKey>>) = System.Linq.Queryable.ThenByDescending(x, keySelector)
 
 
