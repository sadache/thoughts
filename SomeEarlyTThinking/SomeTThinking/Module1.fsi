module Module1
open System
open Exp
type Value
type Env={bindigs:Map<Name,Value> ; context: ExecutionContext}
val eval: Env -> (Exp -> Value)
val monthsAway: DateTime -> int
val clearCache: (unit->unit)
