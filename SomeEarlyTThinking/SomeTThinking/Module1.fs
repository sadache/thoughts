module Module1
open System
open System.Collections.Generic

let getOrElse o (a:'a Lazy)= if(Option.isSome o) then o.Value else a.Value

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

let buildContextKey rawkey ctx = match ctx with 
                                   CellContext((entityname, year, month), _) -> String.Format("{0}.{1}.{2}.{3}", entityname, rawkey, year, month)
                                   | GlobalContext -> rawkey

let applyContextTrans (contextTrans:ContextTrans) ctx = contextTrans ctx


type Exp = Const of double
           |ConstB of bool 
           |Binding of Name * ContextTrans
           |Children of Fold * Exp
           |BinaryExp of Operation * Exp * Exp 
           |If of Exp * Exp * Exp
           |Fun of Name * Exp
           |App of Exp * Exp
and DoubleOp=  Plus | Minus | Times | Min | Max 
and BoolOp= Or | And
and ComparaOp= Equals | Greater | GreaterOrEq 
and Operation= DoubleOp of DoubleOp
              |BoolOp of BoolOp
              |ComparaOp of ComparaOp
and Fold = Sum
           | Avg

type Env= Map<Name,Value>
and Value= DoubleVal of double
            | BoolVal of bool
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


type CalcMap = class
    val _cells : Dictionary<Name, Cell>
    val _cellsDependents : DependencyTree
    val _cellsDependees : DependencyTree
    
    member x.update name cell = 
        x._cells.Item(name) <- cell
    
    member x.findByKey name =
        if x._cells.ContainsKey(name)
                    then Some(x._cells.Item(name))
                    else None
    
    member x.registerVal context rawkey value = 
        let ctxKey = buildContextKey rawkey context
        in
        x._cells.Add(ctxKey, FixedCell(value))

    member x.registerExp context rawkey exp dependees = 
        let ctxKey = buildContextKey rawkey context
        in
        x._cells.Add(ctxKey, NonValuedCell(exp))
        x.setNodeDependees context ctxKey dependees
        x.setNodeAsDependentToDependees context ctxKey dependees
    
    member x.changeTo context rawkey exp dependees = 
        let ctxKey = buildContextKey rawkey context
        in
        let preChangeDependents =
            match (x._cellsDependents.Item(ctxKey)) with
            Node(_, adj) -> adj
        x.removeNodeDependencies context ctxKey
        x.setNodeDependees context ctxKey dependees
        x.setNodeAsDependentToDependees context ctxKey dependees
        x._cells.Item(ctxKey) <- NonValuedCell(exp)
        x.propagateChangeToDependents preChangeDependents
        
    member private x.setNodeDependees context ctxKey (dependees: Exp list) =
        x._cellsDependees.Item(ctxKey) <- Node(ctxKey, [])
        let rec setDependees ctx deps =
            match deps with
            head::tail -> 
                        x.setNodeDependee ctxKey head ctx 
                        setDependees ctx tail
            |[] -> ()
        setDependees context dependees
    
    member private x.setNodeDependee nodeKey bindingasexp context = 
        match(bindingasexp) with
        Binding(name, trans)-> 
                        let newCtx = applyContextTrans trans context
                        in 
                        let ctxKey = buildContextKey name newCtx
                        in                        
                        match x._cellsDependees.Item(nodeKey) with
                        Node(_, adj) -> x._cellsDependees.Item(nodeKey)  <- Node(nodeKey, adj@[ctxKey])
        | Children(_, e) ->  
                                match(context) with
                                CellContext((entityname, year, month), graph) ->
                                        let children = match (getEntityRelations graph entityname) with Owns(list) -> list
                                        in
                                        let rec setChildren list =
                                            match list with
                                            (childentity,_)::t -> 
                                                        x.setNodeDependee nodeKey e (CellContext((childentity, year, month), graph))
                                                        setChildren t
                                            |[]->()
                                        setChildren children
                                |_-> raise(Exception())
        |_-> raise(Exception())
    
    member private x.setNodeAsDependentToDependees context nodeAsDependentCtxKey (dependees: Exp list) =
        let rec setAsDependent ctx deps =
            match deps with
            head::tail -> 
                        x.setNodeAsDependent nodeAsDependentCtxKey head ctx 
                        setAsDependent ctx tail
            |[] -> ()
        setAsDependent context dependees
        

    member private x.setNodeAsDependent nodeAsDependentCtxKey exp context = 
        match(exp) with
        Binding(name, trans)-> 
                        let newCtx = applyContextTrans trans context
                        in 
                        let ctxKey = buildContextKey name newCtx
                        in
                        if (x._cellsDependents.ContainsKey(ctxKey)=false)
                        then x._cellsDependents.Add(ctxKey, Node(ctxKey, [nodeAsDependentCtxKey]))
                        else
                            match (x._cellsDependents.Item(ctxKey)) with
                            Node(_, adj) ->  x._cellsDependents.Item(ctxKey) <- Node(ctxKey, adj@[nodeAsDependentCtxKey])
        | Children(_, e) ->     
                            match(context) with
                            CellContext((entityname, year, month), graph) ->
                                    let children = match (getEntityRelations graph entityname) with Owns(list) -> list
                                    in
                                    let rec setNodeForChildren list =
                                        match list with
                                        (childentity,_)::t -> 
                                                        x.setNodeAsDependent nodeAsDependentCtxKey e (CellContext((childentity, year, month), graph))
                                                        setNodeForChildren t
                                        |[]->()
                                    setNodeForChildren children 
                                |_-> raise(Exception())
        |_ -> raise(Exception())

    
    member private x.removeNodeDependencies context nodeAsDependentCtxKey =
        if x._cellsDependees.ContainsKey(nodeAsDependentCtxKey)
        then 
            let rec removeDep adj =
                match adj with
                head::tail -> 
                        match x._cellsDependents.Item(head) with
                        Node(_, adj) -> x._cellsDependents.Item(head) <- Node(head, List.filter(fun s -> s=nodeAsDependentCtxKey) adj)
                                        removeDep tail
                |[] -> ()
            match x._cellsDependees.Item(nodeAsDependentCtxKey) with
            Node(_, adj) -> removeDep adj
        x._cellsDependees.Item(nodeAsDependentCtxKey) <- Node(nodeAsDependentCtxKey, [])     
    

    member private x.propagateChangeToDependents (dependents:AdjacencyList) =
         let rec propagateChange (dependentsadj:AdjacencyList) = 
            match dependentsadj with
            head::tail -> match x._cells.Item(head) with
                                ValuedCell(value, exp) -> x._cells.Item(head) <- NonValuedCell(exp)
                                |_->()
                          if x._cellsDependents.ContainsKey(head)
                          then    match x._cellsDependents.Item(head) with
                                  Node(_, adj) ->   propagateChange adj
                          propagateChange tail
            |_->()
         propagateChange dependents

    new() = { _cells = new Dictionary<Name, Cell>() 
              _cellsDependents = new Dictionary<TreeNodeKey, TreeNode>()
              _cellsDependees = new Dictionary<TreeNodeKey, TreeNode>() }            
    end
        

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



let rec eval env (cmap:CalcMap) (ctx:MatrixContext) = 
        let gotogetExp name = cmap.findByKey(name)
        let updateCell (cmap:CalcMap) name c = cmap.update name c
        let gotogetValue name trans (env:Env) = let newCtx = trans ctx
                                                let ctxQuery = buildContextKey name newCtx
                                                //attention "Option.get" here raises exception with no functional meaning
                                                let cell = let firstTry=gotogetExp ctxQuery in 
                                                             firstTry |> getOrElse <| lazy((gotogetExp name).Value) in 
                                                    match(cell) with
                                                            ValuedCell(v, _) -> v
                                                            | NonValuedCell(e) -> 
                                                                    let newEnv= ctxIntoEnv newCtx env
                                                                    let v = eval newEnv cmap newCtx e
                                                                    updateCell cmap ctxQuery (ValuedCell(v, e))
                                                                    v
                                                            | FixedCell(v) -> v
                                                            | EmptyCell -> DoubleVal(0.)

        let app op a b = match((eval (ctxIntoEnv ctx env) cmap ctx a,eval (ctxIntoEnv ctx env) cmap ctx b)) with
                                                    (DoubleVal(d1),DoubleVal(d2))-> DoubleVal (op d1 d2)
                                                    |_ -> raise(Exception())
        let compare op a b = match((eval (ctxIntoEnv ctx env) cmap ctx a,eval (ctxIntoEnv ctx env) cmap ctx b)) with
                                                    (DoubleVal(d1),DoubleVal(d2))-> BoolVal (op d1 d2)
                                                    |_ -> raise(Exception())
        in
        function 
                 Const (d)-> DoubleVal d
                 | ConstB b -> BoolVal b
                 | Binding (name, trans) -> let newEnv= ctxIntoEnv ctx env
                                            newEnv.TryFind(name) |> getOrElse <| lazy(gotogetValue name trans newEnv)
                 | Children(fold, e) -> 
                                            let ((entityname,year,month),entitygraph)  = match ctx with
                                                                                         CellContext((n, y, m), g) -> ((n, y, m), g)
                                                                                         |_->raise(Exception())
                                            in
                                            let ownership = match getEntityRelations entitygraph entityname with Owns(list) -> list
                                            in
                                            match fold with
                                            Sum -> 
                                                let rec foldsum list acc =
                                                    match list with
                                                    (owned,r)::t -> 
                                                        let newCtx = CellContext((owned, year, month), entitygraph)
                                                        let newEnv= ctxIntoEnv newCtx env
                                                        match (eval newEnv cmap newCtx e) with
                                                        DoubleVal(d1) -> foldsum t (acc + (d1*r))
                                                        |_->raise(Exception())
                                                    |[]-> DoubleVal(acc)
                                                foldsum ownership 0.
                                            | Avg -> raise(Exception())
                 | BinaryExp(DoubleOp d, e1, e2) -> let op = match d with Plus -> (+) |Times -> (*) | Minus -> (-) | Min -> min | Max -> max
                                                    app op e1 e2
                 | BinaryExp(BoolOp b, e1, e2) -> raise(Exception())
                 | BinaryExp(ComparaOp c, e1, e2) -> let op = match c with Equals -> (=) | Greater -> (>) | GreaterOrEq -> (>=)
                                                     compare op e1 e2
                 | If(ePred, eThen, eElse) -> match (eval (ctxIntoEnv ctx env) cmap ctx ePred) with
                                              BoolVal(res) -> if res 
                                                              then (eval (ctxIntoEnv ctx env) cmap ctx eThen)
                                                              else (eval (ctxIntoEnv ctx env) cmap ctx eElse)
                                              |_ -> raise(Exception())
                 | Fun(name,e)-> FunVal(env,name,e)
                 | App (ef,e1) ->  match(eval (ctxIntoEnv ctx env) cmap ctx ef) with
                                     FunVal(env,name,e)->
                                        let newEnv=Map.add name (eval (ctxIntoEnv ctx env) cmap ctx e1) env
                                        in eval newEnv cmap ctx e
                                     |_ -> raise (Exception())


let (.=.) a b = BinaryExp (ComparaOp Equals, a , b)
let (.>.) a b = BinaryExp (ComparaOp Greater, a , b)
let (.>=.) a b = BinaryExp (ComparaOp GreaterOrEq, a , b)
let (.-.) a b = BinaryExp (DoubleOp Minus, a, b)
let (.+.) a b = BinaryExp (DoubleOp Plus, a,b)
let (.*.) a b = BinaryExp (DoubleOp Times, a,b)
let maxE a b = BinaryExp (DoubleOp Max, a,b)
let minE a b = BinaryExp (DoubleOp Min, a,b)
let ($) = appN

let something = let one=1 in one

let nulContextTrans (c:MatrixContext) = c
let previousYearTrans (c:MatrixContext) = match c with
                                          CellContext((entityname, year, month), g) -> CellContext((entityname, year-1., month), g)
                                          | _ -> c

let var a = Binding(a, nulContextTrans)

// Samples
 
let calcMap = CalcMap()
let env0:Env= Map.empty


let entityGraph = EntityDependencyGraph()
entityGraph.Add("CompanyZ", Owns([]))
entityGraph.Add("CompanyW", Owns([]))
entityGraph.Add("CompanyA", Owns([]))
entityGraph.Add("Holding", Owns [("CompanyA", 0.60)])
entityGraph.Add("OtherHolding", Owns [("CompanyZ", 0.10); ("CompanyW", 0.20)])

let cellCtx entityName (year:int) (month:int) = CellContext((entityName, Convert.ToDouble(year), Convert.ToDouble(month)), entityGraph)

calcMap.registerVal (cellCtx "entity1" 2009 1) "rent" (DoubleVal(100.))
calcMap.registerVal (cellCtx "entity1" 2013 1) "rent" (DoubleVal(200.))
calcMap.registerExp (cellCtx "entity1" 2010 1) "charges" (Const(-10.)) []
calcMap.registerExp (cellCtx "entity1" 2013 1) "charges" (Const(-50.)) []
calcMap.registerExp (cellCtx "entity1" 2010 1) "works" (Const(-20.)) []
calcMap.registerVal (cellCtx "entity1" 2014 1) "rent" (DoubleVal(200.))
calcMap.registerVal (cellCtx "entity1" 2014 1) "charges" (DoubleVal(-50.))
calcMap.registerVal (cellCtx "entity1" 2015 1) "rent" (DoubleVal(200.))
calcMap.registerVal (cellCtx "entity1" 2015 1) "charges" (DoubleVal(-50.))
calcMap.registerVal (cellCtx "entity1" 2016 1) "rent" (DoubleVal(200.))
calcMap.registerVal (cellCtx "entity1" 2016 1) "charges" (DoubleVal(-50.))


calcMap.registerExp GlobalContext "natureX" (funN ["a"; "b"] (var "a" .+. var "b") $ [var "charges"; var "rent"]) [var "charges"; var "rent"]
calcMap.registerExp (cellCtx "entity1" 2010 1) "natureX" (funN ["a"; "b"; "c"] (var "a" .+. var "b" .+. var "c") $ [var "charges"; Binding("rent", previousYearTrans); var "works"]) [var "charges"; Binding("rent", previousYearTrans); var "works"]
calcMap.registerExp (cellCtx "entity1" 2011 1) "natureX" (Binding("natureX", previousYearTrans)) [Binding("natureX", previousYearTrans)]
calcMap.registerExp (cellCtx "entity1" 2012 1) "natureX" (Binding("natureX", previousYearTrans)) [Binding("natureX", previousYearTrans)]

calcMap.registerExp (cellCtx "entity1" 2010 1) "natureYY" (var "natureX") [var "natureX"]
calcMap.registerExp (cellCtx "entity1" 2011 1) "natureYY" (Binding("natureYY", previousYearTrans)) [Binding("natureYY", previousYearTrans)]
calcMap.registerExp (cellCtx "entity1" 2012 1) "natureYY" (Binding("natureYY", previousYearTrans)) [Binding("natureYY", previousYearTrans)]


// simple calculation
let natureX2010 = eval env0 calcMap (cellCtx "entity1" 2010 1) (var "natureX")
// should not be reevaluated twice
let natureX2010_ = eval env0 calcMap (cellCtx "entity1" 2010 1) (var "natureX")

// calculation based on anther cell context (allows time and dimensions browsing)
let natureX2012 = eval env0 calcMap (cellCtx "entity1" 2012 1) (var "natureX")
// again, should not be reevaluated twice
let natureX2012_ = eval env0 calcMap (cellCtx "entity1" 2012 1) (var "natureX")

// a default formula must be used if no formula is defined for the nature & cell context : here natureX from GlobalContext 
let natureX2013 = eval env0 calcMap (cellCtx "entity1" 2013 1) (var "natureX")
let natureX2014 = eval env0 calcMap (cellCtx "entity1" 2014 1) (var "natureX")
let natureX2015 = eval env0 calcMap (cellCtx "entity1" 2015 1) (var "natureX")
let natureX2016 = eval env0 calcMap (cellCtx "entity1" 2016 1) (var "natureX")

// can navigate among natures and time
let natureYY2012 = eval env0 calcMap (cellCtx "entity1" 2012 1) (var "natureYY")

// Can change the formula for a given cell. Dependencies must be updated and change must be propagated so that dependents are recalculated
calcMap.changeTo (cellCtx "entity1" 2010 1) "natureX" (funN ["a"; "b"] (var "a" .+. var "b") $ [var "charges"; Binding("rent", previousYearTrans)]) [var "charges"; Binding("rent", previousYearTrans)]
let natureYY2012_ = eval env0 calcMap (cellCtx "entity1" 2012 1) (var "natureYY")


// holding rent is calculated based on children rents, using an aggregation function (Sum)
calcMap.registerVal (cellCtx "CompanyA" 2009 1) "rent" (DoubleVal(100.)) 
calcMap.registerExp (cellCtx "Holding" 2010 1) "rent" (Children(Sum, Binding("rent", previousYearTrans))) [Children(Sum, Binding("rent", previousYearTrans))]
let holdingrent = eval env0 calcMap (cellCtx "Holding" 2010 1) (var "rent")


// holding rent is calculated taking in account ownership ratio of children
calcMap.registerVal (cellCtx "CompanyW" 2009 1) "rent" (DoubleVal(100.)) 
calcMap.registerVal (cellCtx "CompanyZ" 2009 1) "rent" (DoubleVal(900.)) 
calcMap.registerExp (cellCtx "OtherHolding" 2010 1) "rent" (Children(Sum, Binding("rent", previousYearTrans))) [Children(Sum, Binding("rent", previousYearTrans))]
let otherholdingrent = eval env0 calcMap (cellCtx "OtherHolding" 2010 1) (var "rent")

// allow using YEAR in the formula as a reference to the contextual date
let testYEAR = eval env0 calcMap (cellCtx "Holding" 2010 1) (funN ["a"; "b"] (var "a" .+. var "b") $ [var "YEAR"; Const(10.)])


let testCompare = eval env0 calcMap (cellCtx "Holding" 2010 1) (funN ["a"; "b"] (var "a" .>. var "b") $ [var "YEAR"; Const(10.)])

let testIf = eval env0 calcMap (cellCtx "Holding" 2010 1) (If(var "YEAR" .>. Const(10.), var "YEAR", Const(10.)))

