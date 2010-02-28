module UsefulStuff        
open System
let getOrElse o (a:'a Microsoft.FSharp.Control.Lazy)= if(Option.isSome o) then o.Value else a.Value
let curry2 f = fun a b -> f(a,b)
let reverse f = fun b a-> f a b
let id i=i
let force:Microsoft.FSharp.Control.Lazy<'a>-> 'a= fun l -> l.Force()
 

module Parallels=
    open System
    open System.Collections.Generic
    open System.Linq
    let map f xs= Seq.ofArray( Async.Parallel (List.map (fun x->async{return f x}) xs) |> Async.RunSynchronously)
    let map1 (f:'a->'b) (xs:'a seq):'b seq= System.Linq.ParallelEnumerable.Select(ParallelEnumerable.AsParallel(xs),f).ToList().AsEnumerable()

