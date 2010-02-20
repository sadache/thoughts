#load "UsefulStuff.fs"
#load "Exp.fs"
#load "../Agents/AgentSystem.fs"
#load "Module1.fs"


open Module1
open Exp
open System
// Samples
 
//let calcMap = CalcMap()
let bindings0= Map.empty
let env0With context= {bindigs= bindings0; context=context}
//TODO tests need to be refactored for last big changes
(*
let entityGraph = EntityDependencyGraph()
entityGraph.Add("CompanyZ", Owns([]))
entityGraph.Add("CompanyW", Owns([]))
entityGraph.Add("CompanyA", Owns([]))
entityGraph.Add("Holding", Owns [("CompanyA", 0.60)])
entityGraph.Add("OtherHolding", Owns [("CompanyZ", 0.10); ("CompanyW", 0.20)])

//let cellCtx entityName (year:int) (month:int) = CellContext((entityName, Convert.ToDouble(year), Convert.ToDouble(month)), entityGraph)
let cellCtx entity (year:int) (month:int) = CellContext {entity =entity ;date= monthsAway( DateTime(year,month,1)) ;dependecyFunction= fun _ -> entityGraph}

calcStore.Add (qualifiedKey (cellCtx "entity1" 2009 1, "rent") ,(Const(100.)))
calcStore.Add (qualifiedKey(cellCtx "entity1" 2013 1, "rent"), Const 200.)
calcStore.Add (qualifiedKey(cellCtx "entity1" 2010 1, "charges"), Const -10.) 
calcStore.Add (qualifiedKey(cellCtx "entity1" 2013 1, "charges"),Const -50.) 
calcStore.Add (qualifiedKey(cellCtx "entity1" 2010 1, "works"), Const -20.)
calcStore.Add (qualifiedKey(cellCtx "entity1" 2014 1, "rent"), Const 200.)
calcStore.Add (qualifiedKey (cellCtx "entity1" 2014 1, "charges"),Const -50.)
calcStore.Add (qualifiedKey (cellCtx "entity1" 2015 1, "rent"),Const 200.)
calcStore.Add (qualifiedKey (cellCtx "entity1" 2015 1, "charges"),Const -50.)
calcStore.Add (qualifiedKey (cellCtx "entity1" 2016 1, "rent"), Const 200.)
calcStore.Add (qualifiedKey (cellCtx "entity1" 2016 1, "charges" ),Const -50.)


calcStore.Add (qualifiedKey (GlobalContext, "natureX"), local "charges" .+. local "rent") 

calcStore.Add (qualifiedKey(cellCtx "entity1" 2010 1, "natureX"), (local "charges" .+. Ref("rent", previousYear) .+. Binding "c") )
calcStore.Add (qualifiedKey(cellCtx "entity1" 2011 1, "natureX"), (Ref("natureX", previousYear)))
calcStore.Add (qualifiedKey(cellCtx "entity1" 2012 1, "natureX"), (Ref("natureX", previousYear)))

calcStore.Add (qualifiedKey(cellCtx "entity1" 2010 1, "natureYY"), (local ("natureX")) )
calcStore.Add (qualifiedKey(cellCtx "entity1" 2011 1, "natureYY"), (Ref("natureYY", previousYear)))
calcStore.Add (qualifiedKey(cellCtx "entity1" 2012 1, "natureYY"), (Ref("natureYY", previousYear)) )


// simple calculation
let natureX2010 = eval <| env0With (cellCtx "entity1" 2010 1) <|(local "natureX")
// should not be reevaluated twice
let natureX2010_ = eval <| env0With (cellCtx "entity1" 2010 1) <| (local "natureX")

// calculation based on anther cell context (allows time and dimensions browsing)
let natureX2012 = eval <| env0With (cellCtx "entity1" 2012 1) <|(local "natureX")
// again, should not be reevaluated twice
let natureX2012_ = eval <| env0With (cellCtx "entity1" 2012 1) <| (local "natureX")

// a default formula must be used if no formula is defined for the nature & cell context : here natureX from GlobalContext 

let natureX2013 = eval <| env0With (cellCtx "entity1" 2013 1) <| (local "natureX")
let natureX2014 = eval <| env0With (cellCtx "entity1" 2014 1) <| (local "natureX")
let natureX2015 = eval <| env0With (cellCtx "entity1" 2015 1) <| (local "natureX")
let natureX2016 = eval <| env0With (cellCtx "entity1" 2016 1) <| (local "natureX")

// can navigate among natures and time

let natureYY2012 = eval <| env0With (cellCtx "entity1" 2012 1) <| (local "natureYY")

// Can change the formula for a given cell. Dependencies must be updated and change must be propagated so that dependents are recalculated
calcStore.[qualifiedKey (cellCtx "entity1" 2010 1, "natureX")] <- ( local "charges" .+. Ref("rent", previousYear))
let natureYY2012_ = eval <| env0With (cellCtx "entity1" 2012 1) <| (local "natureYY")


// holding rent is calculated based on children rents, using an aggregation function (Sum)
calcStore.Add (qualifiedKey(cellCtx "CompanyA" 2009 1, "rent"), Const 100.) 
calcStore.Add (qualifiedKey(cellCtx "Holding" 2010 1, "rent"), (Children(Sum, Ref("rent", previousYear)))) 
let holdingrent = eval <| env0With (cellCtx "Holding" 2010 1) <| (local "rent")


// holding rent is calculated taking in account ownership ratio of children
calcStore.Add (qualifiedKey(cellCtx "CompanyW" 2009 1, "rent"), Const 100.) 
calcStore.Add (qualifiedKey(cellCtx "CompanyZ" 2009 1, "rent"), Const 900.) 
calcStore.Add (qualifiedKey(cellCtx "OtherHolding" 2010 1, "rent"), (Children(Sum, Ref("rent", previousYear))))
let otherholdingrent = eval <| env0With (cellCtx "OtherHolding" 2010 1)<| (local "rent")

// allow using YEAR in the formula as a reference to the contextual date
let testYEAR = eval <| env0With (cellCtx "Holding" 2010 1) <| Context Year .+. Const(10.)



*)