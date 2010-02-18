module Exp
open System.Collections.Generic
open UsefulStuff
open System

type Entity = EntityName 
and EntityName= string
type Year = double //...
type Month = double //...

type EntityDependencyGraph = Dictionary<Entity, OwnershipRelations>
and OwnershipRelations = Owns of (Entity * OwnershipRatio) list
and OwnershipRatio = double

type MatrixContext = CellContext of Dimensions * EntityDependencyGraph
                     | GlobalContext
and Dimensions = EntityName * Year * Month
and RelativeEntityTypeLevel = int
and ContextTrans = MatrixContext -> MatrixContext
type Name= string
type ContextDimensions= Year| Month



type  Exp = Const of double
             |ConstB of bool 
             |Context of ContextDimensions
             |Ref of Name * ContextTrans //refs are evil
             |BinaryExp of Operation * Exp * Exp 
             |Binding of Name
             |Children of Fold * Exp            
             |Fun of Name * Exp
             |App of Exp * Exp

and DoubleOp=  Plus |Times |Min |Max 
and BoolOp= Or | And
and ComparaOp= Equals

and Operation= DoubleOp of DoubleOp
               |BoolOp of BoolOp
               |ComparaOp of ComparaOp

and Fold = Sum | Avg

//Some shortcuts
let funN = List.foldBack <| curry2 Fun 
let appN f exps= (List.foldBack <| fun arg f' -> App( f', arg)) exps f
// could do: let appN1= reverse (List.foldBack <| reverse (curry2 App)) 

let max (a:double) (b:double) = Math.Max(a,b)
let min (a:double) (b:double) = Math.Min(a,b)
let (<+>) a b = BinaryExp  (DoubleOp Plus, a,b)
let ($) = appN


let nulContextTrans: ContextTrans  = id
let previousYearTrans :ContextTrans = 
                    function CellContext((entityname, year, month), g) -> CellContext((entityname, year-1., month), g)
                            | c -> c

let globalTrans _= GlobalContext
let local a = Ref(a, nulContextTrans)