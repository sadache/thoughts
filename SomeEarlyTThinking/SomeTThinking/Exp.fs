module Exp
open System.Collections.Generic
open UsefulStuff
open System

type DateD=int

type Entity = {name:EntityName;etype:EntityType;dateBounds:DateD*DateD}
                member x.OutOfBound (d:DateD):bool= let left,right= x.dateBounds in d < left || right < d
and EntityName= string
and EntityType=string

type EntityDependencyGraph = Dictionary<EntityName, Entity*OwnershipRelations>
and OwnershipRelations = Owns of (Entity * OwnershipRatio) list
and OwnershipRatio = double
and EntityDependencyFunction= DateD -> EntityDependencyGraph
let getEntityRelations (entityGraph:EntityDependencyGraph) entity = entityGraph.Item(entity)


type ExecutionContext = CellContext of Dimensions * EntityDependencyFunction
                            member x.IsConsistent= match x with CellContext (ds,_) -> not(ds.entity.OutOfBound(ds.date))
                            member x.EntityDependencies= match x with CellContext (ds, eDepFunc) -> getEntityRelations (eDepFunc(ds.date)) ds.entity.name



and Dimensions = {entity :Entity ;date: DateD}
                    
type AttachementLevel=Cell of Dimensions 
                      |Partial of PartialDimensions
                      | Global 


and PartialDimensions={entityType:EntityType Option(* ; add other optional dimensions *) }                   

and ContextTrans = ExecutionContext -> ExecutionContext
type Name= string
type DimensionsRequest= Year| Month



type  Exp = Const of double
             |ConstB of bool 
             |Context of DimensionsRequest
             |Ref of Name * ContextTrans //refs are evil
             |BinaryExp of Operation * Exp * Exp 
             |If of Exp * Exp * Exp
             |Binding of Name
             |Children of Fold * Exp            
             |Fun of Name * Exp
             |App of Exp * Exp

and DoubleOp=  Plus |Minus|Times |Min |Max 
and BoolOp= Or | And
and ComparaOp= Equals|Greater | GreaterOrEq

and Operation= DoubleOp of DoubleOp
               |BoolOp of BoolOp
               |ComparaOp of ComparaOp

and Fold = Sum | Avg

//Some shortcuts
let funN = List.foldBack <| curry2 Fun 
let appN f exps= (List.foldBack <| fun arg f' -> App( f', arg)) exps f
// could do: let appN1= reverse (List.foldBack <| reverse (curry2 App)) 

let (.=.) a b = BinaryExp (ComparaOp Equals, a , b)
let (.>.) a b = BinaryExp (ComparaOp Greater, a , b)
let (.>=.) a b = BinaryExp (ComparaOp GreaterOrEq, a , b)
let (.-.) a b = BinaryExp (DoubleOp Minus, a, b)
let (.+.) a b = BinaryExp (DoubleOp Plus, a,b)
let (.*.) a b = BinaryExp (DoubleOp Times, a,b)
let maxE a b = BinaryExp (DoubleOp Max, a,b)
let minE a b = BinaryExp (DoubleOp Min, a,b)


let nulContextTrans: ContextTrans  = id
let previousYear :ContextTrans = function CellContext(ds, entitydepFun) -> CellContext( {ds with date=ds.date - 12}, entitydepFun ) 
let previousMonth :ContextTrans = function CellContext(ds, entitydepFun) -> CellContext( {ds with date=ds.date - 1}, entitydepFun ) 


let local a = Ref(a, nulContextTrans)