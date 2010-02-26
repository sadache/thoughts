module Module1
open System
open System.Collections.Generic
open Exp
open UsefulStuff
open AgentSystem.LAgent

let  minT:int ref =ref 0
let  minIOT:int ref =ref 0
let _= System.Threading.ThreadPool.GetMinThreads(minT,minIOT)
let _= System.Threading.ThreadPool.SetMinThreads(100,minIOT.contents)


let mutable evaluatedCells=0
let referenceDate= DateTime(2010,1,1).AddMonths(-1)
let monthsAway (date:DateTime)= 12 * (date.Year - referenceDate.Year) + date.Month - referenceDate.Month

let buildContextKey rawkey  =
             function Cell(ds) -> String.Format("{0}.{1}.{2}", ds.entity.name, rawkey, ds.date)
                      |Partial ods -> 
                        String.Format("{0}.{1}", defaultArg ods.entityType "_", rawkey)
                      | Global -> rawkey


type Env= {bindigs:Map<Name,Value> ; context: ExecutionContext}

and Value= DoubleVal of double
            |BoolVal of bool
            |FunVal of Env * Name * Exp

type CalcStore=Dictionary<string,Exp>
type QualifiedName=  ExecutionContext * String
open Microsoft.FSharp.Control
let rec collectExDependencies exp (ctxt:ExecutionContext)= 
        let rec collect dependencies= 
                   function |_ when not (ctxt.IsConsistent) -> dependencies
                            |Const _|ConstB _| Binding _ |Context _->  dependencies
                            |Ref(name,trans)->  (name,trans ctxt)::dependencies
                            |BinaryExp(_,e1,e2) |App (e1,e2)-> collect (collect dependencies e1) e2
                            |Fun(_,e) ->  collect dependencies e
                            |Children(fold, e) -> 
                                            let dims, entitydepFun  = match ctxt with CellContext(d, entitydepFun) -> d, entitydepFun
                                            let (_,Owns(ownership)) =ctxt.EntityDependencies 
                                            in List.fold (fun ds (owned,_)-> let newCtx = CellContext({dims with entity=owned}, entitydepFun)
                                                                             in ds @ (collectExDependencies e newCtx) ) dependencies ownership  
                            |If (condition,_,_) -> collect dependencies condition
        collect [] exp
                     
let mutable isOk= 0

let rec eval (env :Env) = 
  let app op a b = match(eval env a,eval env b) with
                         DoubleVal(d1),DoubleVal(d2)->  (op d1 d2)
                         |v1,v2 -> raise <| InvalidProgramException(String.Format ( "cannot add apply {0} to {1} and {2} " , op,v1,v2 ))
            
  in function|_ when not env.context.IsConsistent -> DoubleVal 0.
             |Const d-> DoubleVal d
             |ConstB b-> BoolVal b
             |Context dimension-> 
                match dimension,env.context with Year ,CellContext(ds,_) ->  DoubleVal (Convert.ToDouble( referenceDate.AddMonths(ds.date).Year)) 
                                                |Month ,CellContext(ds,_) ->  DoubleVal (Convert.ToDouble( referenceDate.AddMonths(ds.date).Month)) 
                                                                                                       
             |Ref(name,trans)-> evaluatedCells <- evaluatedCells+1
                                (gotogetValue name trans env)  
             |Children(fold, e) ->  let ds, entitydepFun  = match env.context with CellContext(d, f) -> d, f
                                    let (_,Owns(ownership)) =env.context.EntityDependencies 
                                    let childrenEvaluated= let map= if(true)then let _= isOk<-isOk+1 in Parallels.map else Seq.map
                                                           in  map  (fun(owned,r) -> let  newCtx = CellContext({ds with entity=owned}, entitydepFun)
                                                                                     let newEnv= {env with context= newCtx}
                                                                                     in((eval newEnv e),r) ) ownership                                      
                                                                        
                                    in match fold with
                                          Sum ->  DoubleVal( Seq.fold (fun (s) (DoubleVal(d1),rate) ->  s + d1*rate ) 0. (childrenEvaluated))
                                          | Avg -> raise(Exception())

             |BinaryExp (DoubleOp o, e1,e2)-> let op = match o with Plus -> (+) |Times -> (*) |Minus -> (-)| Min -> min | Max -> max 
                                              in DoubleVal(app op e1 e2)
             |BinaryExp (ComparaOp o, e1,e2)-> let op = match o with Equals -> (=) |Greater -> (>) | GreaterOrEq -> (>=) 
                                               in BoolVal(app op e1 e2)
             |BinaryExp (BoolOp o, e1,e2)->  raise(NotImplementedException())
             |If (condition , eThen , eElse)->match (eval env condition) with
                                                BoolVal(res) -> if res then (eval env  eThen) else (eval env eElse)
                                                |other-> raise(InvalidProgramException(String.Format ("{0} is not a boolean expression", ([|other|]:Object[]))))
             |Binding name -> env.bindigs.TryFind name |> getOrElse <| lazy(raise <| InvalidProgramException ("Binding to unexisting name " + name))
             |Fun(name,e)-> FunVal(env,name,e)
             |App (ef,e1) ->  match(eval env ef) with
                                 FunVal(env,name,e)->
                                    let newEnv= {env with bindigs= env.bindigs.Add(name, (eval env e1))}
                                    in eval newEnv e
                                 |somethingElse -> raise <| InvalidProgramException(String.Format ( "{0} is not a function to be applied" , somethingElse ))

and gotogetValue name trans (env:Env) = let newEnv = {env with  context=trans env.context }
                                        if(not(newEnv.context.IsConsistent)) then DoubleVal 0.
                                        else let task= (storeCache (name, newEnv.context) newEnv.bindigs )
                                             in (snd <| task.Force())

and calcStore= new Dictionary<string,Exp>()
and getCalcFromStore qualifiedKey= if calcStore.ContainsKey qualifiedKey then Some calcStore.[qualifiedKey] else None
and qualifiedKey (context,key)= buildContextKey key context

and cache= System.Collections.Concurrent. ConcurrentDictionary<string,Lazy<Set<string>*Value>>(100,10000) 

and storeCache (key,context) env :Lazy<Set<string>*Value>=     
    let qualifiedKey= match context with CellContext(ds,_)-> buildContextKey key (Cell(ds))
    let valueFactory= fun k->lazy(let entityT= match context with CellContext(ds,_)->ds.entity.etype 
                                  let partialKey= buildContextKey key (Partial {entityType=Some entityT})
                                  let exp= getCalcFromStore qualifiedKey |> getOrElse <| lazy((getCalcFromStore partialKey) |> getOrElse <| lazy( defaultArg (getCalcFromStore key) (Const 0.)))
    
                                  //let dependencies=collectExDependencies exp context
                                  let newEnv={bindigs=env ;context=context}
                                  //let _=  worker <-- ( fun()-> List.iter (reverse storeCache env >> ignore)    dependencies) 
                                  //I need to store dependencies rather with EXP rather than with values
                                  in (Set.empty(*Set.ofList(List.map (fun (key,cxt)->buildContextKey key cxt) dependencies)*),eval newEnv exp))
    in cache.GetOrAdd(qualifiedKey,valueFactory)
                                                              
//and worker = spawnParallelWorker (fun f -> //printfn "doing some Work"
 //                                          f()) 10
(* open AgentSystem.LAgent
let agent =
   Agent.Start(fun inbox ->
     async { while true do
               let! msg = inbox.Receive()
               printfn "got message '%s'" msg } ) *)

