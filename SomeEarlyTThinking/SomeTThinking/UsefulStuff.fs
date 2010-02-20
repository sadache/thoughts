module UsefulStuff        
open System
let getOrElse o (a:'a Lazy)= if(Option.isSome o) then o.Value else a.Value
let curry2 f = fun a b -> f(a,b)
let reverse f = fun b a-> f a b
let id i=i

