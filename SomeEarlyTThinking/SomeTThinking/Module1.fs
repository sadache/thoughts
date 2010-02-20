module Module1
open System
open System.Collections.Generic
open Exp
open UsefulStuff
open AgentSystem.LAgent

let referenceDate= DateTime(2010,1,1).AddMonths(-1)
let monthsAway (date:DateTime)= 12 * (date.Year - referenceDate.Year) + date.Month - referenceDate.Month
let getEntityRelations (entityGraph:EntityDependencyGraph) entity = entityGraph.Item(entity)

let buildContextKey rawkey  =
             function CellContext(ds) -> String.Format("{0}.{1}.{2}", ds.entity.name, rawkey, ds.date)
                      |PartialContext ods -> 
                        String.Format("{0}.{1}.{2}", defaultArg ods.entityType "_", rawkey, "_")
                      | GlobalContext -> rawkey

let applyContextTrans (contextTrans:ContextTrans) ctx = contextTrans ctx

type Env= {bindigs:Map<Name,Value> ; context: MatrixContext}

and Value= DoubleVal of double
            |BoolVal of bool
            | FunVal of Env * Name * Exp

type CalcStore=Dictionary<string,Exp>
type QualifiedName=  MatrixContext * String
open Microsoft.FSharp.Control
type Promise<'a>= System.Threading.Tasks.Task<'a>
let rec collectExDependencies exp (ctxt:MatrixContext)= 
                let rec collect dependencies= 
                           function  _ when not (ctxt.IsConsistent) -> dependencies
                                    |Const _|ConstB _| Binding _ |Context _->  dependencies
                                    |Ref(name,trans)->  (name,trans ctxt)::dependencies
                                    |BinaryExp(_,e1,e2) |App (e1,e2)-> collect (collect dependencies e1) e2
                                    |Fun(_,e) ->  collect dependencies e
                                    |Children(fold, e) -> let dims  = match ctxt with CellContext(d) -> (d)
                                                                                    |_->raise(Exception())
                                                          let ownership = match getEntityRelations (dims.dependecyFunction(dims.date)) dims.entity.name with (_,Owns(list)) -> list
                                                          in List.fold (fun ds (owned,_)-> let newCtx = CellContext({dims with entity=owned})
                                                                                            in ds @ (collectExDependencies e newCtx) ) dependencies ownership  
                                    |If (condition,_,_) -> collect dependencies condition
                let result= collect [] exp
                //let _=printfn "interesting %s" (Seq. fold (fun s (name,mctxt)-> (s + " " + name))  "" result)
                in result
                                        

let rec eval (env :Env) = 
        let gotogetValue name trans (env:Env) = let newEnv = {env with  context=trans env.context }
                                                if(not(newEnv.context.IsConsistent)) then DoubleVal 0.
                                                else let task= (storeCache (name, newEnv.context) newEnv.bindigs )
                                                     //printfn "is Completed %b"  task.IsCompleted
                                                     snd ( task)

        let app op a b = match((eval env a,eval env b)) with
                            (DoubleVal(d1),DoubleVal(d2))->  (op d1 d2)
                            |(v1,v2) -> raise <| InvalidProgramException(String.Format ( "cannot add apply {0} to {1} and {2} " , op,v1,v2 ))
        
        in
        function _ when not env.context.IsConsistent -> DoubleVal 0.
                 | Const (d)-> DoubleVal d
                 |ConstB b-> BoolVal b
                 |Context dimension-> 
                        match (dimension,env.context) with (Year ,CellContext (ds)) ->  DoubleVal (Convert.ToDouble( referenceDate.AddMonths(ds.date).Year)) 
                                                           |(Month ,CellContext (ds)) ->  DoubleVal (Convert.ToDouble( referenceDate.AddMonths(ds.date).Month)) 
                                                           |(_,GlobalContext) |(_,PartialContext _) -> raise (InvalidProgramException("global and partial context does not contain the demanded dimension"))
                                                           
                 |Ref(name,trans)-> (gotogetValue name trans env)  
                 |Binding name -> env.bindigs.TryFind name |> getOrElse <| lazy(raise <| InvalidProgramException ("Binding to unexisting name " + name))
                 |Children(fold, e) ->      let ds  = match env.context with CellContext(d) -> (d)
                                                                               |_->raise(Exception())
                                            let ownership = match getEntityRelations (ds.dependecyFunction(ds.date)) ds.entity.name with (_,Owns(list)) -> list
                                            in
                                            match fold with
                                            
                                            Sum -> 
                                                let rec foldsum list acc =
                                                    match list with
                                                    (owned,r)::t -> 
                                                        let newCtx = CellContext({ds with entity=owned})
                                                        let newEnv= {env with context= newCtx}
                                                        match (eval newEnv e) with
                                                        DoubleVal(d1) -> foldsum t (acc + (d1*r))
                                                        |_->raise(Exception())
                                                    |[]-> DoubleVal(acc)
                                                foldsum ownership 0.
                                            | Avg -> raise(Exception())
                 | BinaryExp (DoubleOp o, e1,e2)-> let op = match o with Plus -> (+) |Times -> (*) |Minus -> (-)| Min -> min | Max -> max 
                                                   in DoubleVal(app op e1 e2)
                 | BinaryExp (ComparaOp o, e1,e2)-> let op = match o with Equals -> (=) |Greater -> (>) | GreaterOrEq -> (>=) 
                                                    in BoolVal(app op e1 e2)
                 | BinaryExp (BoolOp o, e1,e2)->  raise(NotImplementedException())
                 | If (condition , eThen , eElse)->match (eval env condition) with
                                                    BoolVal(res) -> if res then (eval env  eThen) else (eval env eElse)
                                                    |other-> raise(InvalidProgramException(String.Format ("{0} is not a boolean expression", ([|other|]:Object[]))))
                 | Fun(name,e)-> FunVal(env,name,e)
                 | App (ef,e1) ->  match(eval env ef) with
                                     FunVal(env,name,e)->
                                        let newEnv= {env with bindigs= env.bindigs.Add(name, (eval env e1))}
                                        in eval newEnv e
                                     |exp -> raise <| InvalidProgramException(String.Format ( "{0} is not a function to be applied" , exp ))

and calcStore= new Dictionary<string,Exp>()
and getCalcFromStore qualifiedKey= if calcStore.ContainsKey qualifiedKey then Some calcStore.[qualifiedKey] else None
and qualifiedKey (context,key)= buildContextKey key context

and cache= System.Collections.Concurrent.ConcurrentDictionary<string,Set<string>*Value>() 
and storeCache (key,context) env :Set<string>*Value=     
                let qualifiedKey= buildContextKey key context
                let entityT= match context with CellContext ds->ds.entity.etype |_-> ""
                let partialKey= buildContextKey key (PartialContext {entityType=Some entityT})
                let exp= getCalcFromStore qualifiedKey |> getOrElse <| lazy((getCalcFromStore partialKey) |> getOrElse <| lazy( defaultArg (getCalcFromStore key) (Const 0.)))
                let valueFactory= fun k->let dependencies=collectExDependencies exp context
                                         let newEnv={bindigs=env ;context=context}
                                         let _= worker <-- ( fun()->ignore <| List.map (reverse storeCache env)    dependencies)
                                         in (Set.ofList(List.map (fun (key,cxt)->buildContextKey key cxt) dependencies  ),eval newEnv exp)
                in cache.GetOrAdd(qualifiedKey,valueFactory)
                                                              
and worker = spawnParallelWorker (fun f -> //printfn "doing some Work"
                                           f()) 20
(* open AgentSystem.LAgent
let agent =
   Agent.Start(fun inbox ->
     async { while true do
               let! msg = inbox.Receive()
               printfn "got message '%s'" msg } ) *)