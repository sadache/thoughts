module Module1
open System
open System.Collections.Generic
open Exp
open UsefulStuff

let getEntityRelations (entityGraph:EntityDependencyGraph) entityName = entityGraph.Item(entityName)

let buildContextKey rawkey  =
             function CellContext((entityname, year, month), _) -> String.Format("{0}.{1}.{2}.{3}", entityname, rawkey, year, month)
                    | GlobalContext -> rawkey

let applyContextTrans (contextTrans:ContextTrans) ctx = contextTrans ctx

type Env= {bindigs:Map<Name,Value> ; context: MatrixContext}

and Value= DoubleVal of double
            | FunVal of Env * Name * Exp

type CalcStore=Dictionary<string,Exp>
type QualifiedName=  MatrixContext * String
open Microsoft.FSharp.Control
type Promise<'a>= System.Threading.Tasks.Task<'a>
let collectExDependencies exp ctxt= 
                let rec collect dependencies= 
                           function  Const _|ConstB _| Binding _ |Context _->  dependencies
                                    |Ref(name,trans)->  (name,trans ctxt)::dependencies
                                    |BinaryExp(_,e1,e2) |App (e1,e2)-> collect (collect dependencies e1) e2
                                    |Fun(_,e) ->  collect dependencies e
                                    |Children(_,_)-> dependencies//raise(NotImplementedException())

                in collect [] exp
                                        

let rec eval (env :Env) = 
        let gotogetValue name trans (env:Env) = let newEnv = {env with  context=trans env.context }
                                                in storeCache (name, newEnv.context) newEnv.bindigs

        let app op a b = match((eval env a,eval env b)) with
                            (DoubleVal(d1),DoubleVal(d2))-> DoubleVal (op d1 d2)
                            |(v1,v2) -> raise <| InvalidProgramException(String.Format ( "cannot add apply {0} to {1} and {2} " , ([|op  ; v1; v2 |]:Object[]) ))
        in
        function Const (d)-> DoubleVal d
                 |ConstB b-> raise (NotImplementedException())
                 |Context dimension-> match (dimension,env.context) with (Year ,CellContext ((_, y, _), _)) ->  DoubleVal y 
                                                                        |(Month,CellContext ((_, _, m), _)) -> DoubleVal m
                                                                         |(_,GlobalContext) -> raise (InvalidProgramException("global context does not contain the demanded dimension"))
                 |Ref(name,trans)-> snd (gotogetValue name trans env).Result  
                 |Binding name -> env.bindigs.TryFind name |> getOrElse <| lazy(raise <| InvalidProgramException ("Binding to unexisting name " + name))
                 |Children(fold, e) -> 
                                            let ((entityname,year,month),entitygraph)  = match env.context with
                                                                                         CellContext(d, g) -> (d, g)
                                                                                         |_->raise(Exception())
                                            let ownership = match getEntityRelations entitygraph entityname with Owns(list) -> list
                                            in
                                            match fold with
                                            Sum -> //List.fold  fun acc child ->   DoubleVal(0)
                                                let rec foldsum list acc =
                                                    match list with
                                                    (owned,r)::t -> 
                                                        let newCtx = CellContext((owned, year, month), entitygraph)
                                                        let newEnv= {env with context= newCtx}
                                                        match (eval newEnv e) with
                                                        DoubleVal(d1) -> foldsum t (acc + (d1*r))
                                                        |_->raise(Exception())
                                                    |[]-> DoubleVal(acc)
                                                foldsum ownership 0.
                                            | Avg -> raise(Exception())
                 | BinaryExp (DoubleOp o, e1,e2)-> let op = match o with Plus -> (+) |Times -> (*) | Min -> min | Max -> max 
                                                   in app op e1 e2
                 | BinaryExp (ComparaOp o, e1,e2)-> raise(NotImplementedException())
                 | BinaryExp (BoolOp o, e1,e2)->  raise(NotImplementedException())
                 | Fun(name,e)-> FunVal(env,name,e)
                 | App (ef,e1) ->  match(eval env ef) with
                                     FunVal(env,name,e)->
                                        let newEnv= {env with bindigs= env.bindigs.Add(name, (eval env e1))}
                                        in eval newEnv e
                                     |exp -> raise <| InvalidProgramException(String.Format ( "{0} is not a function to be applied" , exp ))

and calcStore= new Dictionary<string,Exp>()
and getCalcFromStore qualifiedKey= if calcStore.ContainsKey qualifiedKey then Some calcStore.[qualifiedKey] else None
and qualifiedKey (context,key)= buildContextKey key context

and cache= System.Collections.Concurrent.ConcurrentDictionary<string,Promise<Set<string>*Value>>() 
and storeCache (key,context) env:Promise<Set<string>*Value>=     
                let qualifiedKey= buildContextKey key context 
                let exp= getCalcFromStore qualifiedKey |> getOrElse <| lazy( Option.get (getCalcFromStore key))
                let valueFactory=fun k-> let toRun  = (async{ let dependencies=collectExDependencies exp context
                                                              let newEnv={bindigs=env ;context=context}
                                                              let! others=  Async.Parallel <| List.map  ( reverse storeCache env >> Async.AwaitTask )  dependencies 
                                                              return (Set.ofList(List.map (fun (key,cxt)->buildContextKey key cxt) dependencies  ),eval newEnv exp)})
                                         in Async.StartAsTask toRun
                in cache.GetOrAdd(qualifiedKey,valueFactory)                                                              



