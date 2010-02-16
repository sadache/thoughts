module Module1
open System
open System.Collections.Generic

let getOrElse o (a:'a Lazy)= if(Option.isSome o) then o.Value else a.Force()

type Entity = EntityName 
and EntityName= string

type EntityDependencyGraph = Dictionary<Entity, OwnershipRelations>
and OwnershipRelations = Owns of (Entity * OwnershipRatio) list
and OwnershipRatio = double

let getEntityRelations (entityGraph:EntityDependencyGraph) entityName = entityGraph.Item(entityName)

type Name= string
type Year = double //...
type Month = double //...



type MatrixContext = CellContext of Dimensions * EntityDependencyGraph
                     | GlobalContext
and Dimensions = EntityName * Year * Month
and RelativeEntityTypeLevel = int
and ContextTrans = MatrixContext -> MatrixContext

let buildContextKey rawkey  =
             function CellContext((entityname, year, month), _) -> String.Format("{0}.{1}.{2}.{3}", entityname, rawkey, year, month)
                    | GlobalContext -> rawkey

let applyContextTrans (contextTrans:ContextTrans) ctx = contextTrans ctx

type Exp = Const of double
           |Ref of Name * ContextTrans //refs are evil
           |Binding of Name
           |Children of Fold * Exp
           |Plus of Exp * Exp
           |Times of Exp * Exp
           |Min of Exp * Exp
           |Max of Exp * Exp
           |Fun of Name * Exp
           |App of Exp * Exp

and Fold = Sum
           | Avg

type Env= Map<Name,Value>
and Value= DoubleVal of double
            | FunVal of Env * Name * Exp

type Cell = EmptyCell
            | ValuedCell of Value * Exp 
            | NonValuedCell of Exp
            | FixedCell of Value 


type DependencyTree = Dictionary<TreeNodeKey, TreeNode>
and TreeNode = Node of TreeNodeKey * AdjacencyList
and TreeNodeKey = string
and AdjacencyList = string list







let ctxIntoEnv ctx env =    let tenv = if (Map.containsKey("YEAR") env) then (Map.remove("YEAR") env) else env
                            let tenv' = if (Map.containsKey("MONTH") tenv) then (Map.remove("MONTH") tenv) else tenv // ... :-(
                            match ctx with
                            CellContext((_, y, m), _) -> Map.add "MONTH" (DoubleVal(m)) (Map.add "YEAR" (DoubleVal(y)) tenv')
                            |_-> env




        

let funN args exp= let rec doArgs args= match args with |(head::[])-> Fun(head,exp)
                                                        |(h::tail) -> Fun(h,doArgs tail)
                                                        |_ -> raise (Exception()) 
                   in doArgs args

let appN f exps= let rec doArgs exps= match exps with   |(head::[])-> App(f, head)
                                                        |(h::tail) -> App(doArgs tail, h)
                                                        |_ -> raise (Exception()) 
                   in doArgs exps


let max (a:double) (b:double) = Math.Max(a,b)
let min (a:double) (b:double) = Math.Min(a,b)


type CalcStore=Dictionary<string,Exp>
type QualifiedName=  MatrixContext * String


let rec eval env  (ctx:MatrixContext) = 
        let gotogetExp name = storeCache name
        let gotogetValue name trans (env:Env) = let newCtx = trans ctx
                                                in gotogetExp (name, newCtx) env

        let app op a b = match((eval (ctxIntoEnv ctx env)  ctx a,eval (ctxIntoEnv ctx env)  ctx b)) with
                                                    (DoubleVal(d1),DoubleVal(d2))-> DoubleVal (op d1 d2)
                                                    |_ -> raise(Exception())
        in
        function 
                 Const (d)-> DoubleVal d
                 |Ref(name,trans)-> let newEnv= ctxIntoEnv ctx env in
                                            newEnv.TryFind(name) |> getOrElse <| lazy(gotogetValue name trans newEnv)
                 | Binding name -> env.TryFind name |> getOrElse <| lazy(raise <| InvalidProgramException ("Binding to unexisting name " + name))
                 | Children(fold, e) -> 
                                            let ((entityname,year,month),entitygraph)  = match ctx with
                                                                                         CellContext((n, y, m), g) -> ((n, y, m), g)
                                                                                         |_->raise(Exception())
                                            let ownership = match getEntityRelations entitygraph entityname with Owns(list) -> list
                                            in
                                            match fold with
                                            Sum -> 
                                                let rec foldsum list acc =
                                                    match list with
                                                    (owned,r)::t -> 
                                                        let newCtx = CellContext((owned, year, month), entitygraph)
                                                        let newEnv= ctxIntoEnv newCtx env
                                                        match (eval newEnv newCtx e) with
                                                        DoubleVal(d1) -> foldsum t (acc + (d1*r))
                                                        |_->raise(Exception())
                                                    |[]-> DoubleVal(acc)
                                                foldsum ownership 0.
                                            | Avg -> raise(Exception())
                 | Plus(e1,e2)-> app (+) e1 e2
                 | Times(e1, e2) -> app (*) e1 e2
                 | Max(e1, e2) -> app max e1 e2
                 | Min(e1, e2) -> app min e1 e2
                 | Fun(name,e)-> FunVal(env,name,e)
                 | App (ef,e1) ->  match(eval (ctxIntoEnv ctx env) ctx ef) with
                                     FunVal(env,name,e)->
                                        let newEnv=Map.add name (eval (ctxIntoEnv ctx env) ctx e1) env
                                        in eval newEnv ctx e
                                     |_ -> raise (Exception())

and calcStore= new Dictionary<string,Exp>()
and getCalcFromStore qualifiedKey= if calcStore.ContainsKey qualifiedKey then Some calcStore.[qualifiedKey] else None
and qualifiedKey (context,key)= buildContextKey key context
and cache= System.Collections.Concurrent.ConcurrentDictionary<string,Value>() 
and storeCache (key,context) env=
            let qualifiedKey= buildContextKey key context in
                                cache.GetOrAdd(qualifiedKey,
                                     fun k-> eval env context 
                                              ( getCalcFromStore qualifiedKey 
                                                 |> getOrElse 
                                                 <| lazy(Option.get (getCalcFromStore key))))

let (<+>) a b = Plus (a,b)
let ($) = appN

let nulContextTrans (c:MatrixContext) = c
let previousYearTrans (c:MatrixContext) = match c with
                                          CellContext((entityname, year, month), g) -> CellContext((entityname, year-1., month), g)
                                          | _ -> c
let globalYearTrans _= GlobalContext
let var a = Ref(a, nulContextTrans)

// Samples
 
//let calcMap = CalcMap()
let env0:Env= Map.empty


let entityGraph = EntityDependencyGraph()
entityGraph.Add("CompanyZ", Owns([]))
entityGraph.Add("CompanyW", Owns([]))
entityGraph.Add("CompanyA", Owns([]))
entityGraph.Add("Holding", Owns [("CompanyA", 0.60)])
entityGraph.Add("OtherHolding", Owns [("CompanyZ", 0.10); ("CompanyW", 0.20)])

let cellCtx entityName (year:int) (month:int) = CellContext((entityName, Convert.ToDouble(year), Convert.ToDouble(month)), entityGraph)

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


calcStore.Add (qualifiedKey (GlobalContext, "natureX"), (funN ["a"; "b"] (var "a" <+> var "b") $ [var "charges"; var "rent"])) 
calcStore.Add (qualifiedKey(cellCtx "entity1" 2010 1, "natureX"), funN ["a"; "b"; "c"] (var "a" <+> var "b" <+> var "c") $ [var "charges"; Ref("rent", previousYearTrans); var "works"])
calcStore.Add (qualifiedKey(cellCtx "entity1" 2011 1, "natureX"), (Ref("natureX", previousYearTrans)))
calcStore.Add (qualifiedKey(cellCtx "entity1" 2012 1, "natureX"), (Ref("natureX", previousYearTrans)))

calcStore.Add (qualifiedKey(cellCtx "entity1" 2010 1, "natureYY"), (var ("natureX")) )
calcStore.Add (qualifiedKey(cellCtx "entity1" 2011 1, "natureYY"), (Ref("natureYY", previousYearTrans)))
calcStore.Add (qualifiedKey(cellCtx "entity1" 2012 1, "natureYY"), (Ref("natureYY", previousYearTrans)) )


// simple calculation
let natureX2010 = eval env0 (cellCtx "entity1" 2010 1) (var "natureX")
// should not be reevaluated twice
let natureX2010_ = eval env0 (cellCtx "entity1" 2010 1) (var "natureX")

// calculation based on anther cell context (allows time and dimensions browsing)
let natureX2012 = eval env0 (cellCtx "entity1" 2012 1) (var "natureX")
// again, should not be reevaluated twice
let natureX2012_ = eval env0 (cellCtx "entity1" 2012 1) (var "natureX")

// a default formula must be used if no formula is defined for the nature & cell context : here natureX from GlobalContext 

let natureX2013 = eval env0 (cellCtx "entity1" 2013 1) (var "natureX")
let natureX2014 = eval env0 (cellCtx "entity1" 2014 1) (var "natureX")
let natureX2015 = eval env0 (cellCtx "entity1" 2015 1) (var "natureX")
let natureX2016 = eval env0 (cellCtx "entity1" 2016 1) (var "natureX")

// can navigate among natures and time

let natureYY2012 = eval env0 (cellCtx "entity1" 2012 1) (var "natureYY")

// Can change the formula for a given cell. Dependencies must be updated and change must be propagated so that dependents are recalculated
calcStore.[qualifiedKey (cellCtx "entity1" 2010 1, "natureX")] <- (funN ["a"; "b"] (var "a" <+> var "b") $ [var "charges"; Ref("rent", previousYearTrans)])
let natureYY2012_ = eval env0 (cellCtx "entity1" 2012 1) (var "natureYY")


// holding rent is calculated based on children rents, using an aggregation function (Sum)
calcStore.Add (qualifiedKey(cellCtx "CompanyA" 2009 1, "rent"), Const 100.) 
calcStore.Add (qualifiedKey(cellCtx "Holding" 2010 1, "rent"), (Children(Sum, Ref("rent", previousYearTrans)))) 
let holdingrent = eval env0 (cellCtx "Holding" 2010 1) (var "rent")


// holding rent is calculated taking in account ownership ratio of children
calcStore.Add (qualifiedKey(cellCtx "CompanyW" 2009 1, "rent"), Const 100.) 
calcStore.Add (qualifiedKey(cellCtx "CompanyZ" 2009 1, "rent"), Const 900.) 
calcStore.Add (qualifiedKey(cellCtx "OtherHolding" 2010 1, "rent"), (Children(Sum, Ref("rent", previousYearTrans))))
let otherholdingrent = eval env0 (cellCtx "OtherHolding" 2010 1) (var "rent")

// allow using YEAR in the formula as a reference to the contextual date
let testYEAR = eval env0 (cellCtx "Holding" 2010 1) (funN ["a"; "b"] (var "a" <+> var "b") $ [var "YEAR"; Const(10.)])



