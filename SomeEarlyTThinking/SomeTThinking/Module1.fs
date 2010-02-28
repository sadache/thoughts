module Module1
open System
open System.Collections.Generic
open Exp
open ExpStore
open UsefulStuff
open AgentSystem.LAgent

let  minT:int ref =ref 0
let  minIOT:int ref =ref 0
let _= System.Threading.ThreadPool.GetMinThreads(minT,minIOT)
let _= System.Threading.ThreadPool.SetMinThreads(100,minIOT.contents)


let mutable evaluatedCells=0
let referenceDate= DateTime(2010,1,1).AddMonths(-1)
let monthsAway (date:DateTime)= 12 * (date.Year - referenceDate.Year) + date.Month - referenceDate.Month


type Env= {bindigs:Map<Name,Value> ; context: ExecutionContext}

and Value= DoubleVal of double
            |BoolVal of bool
            |FunVal of Env * Name * Exp

type QualifiedName=  ExecutionContext * String

let searchHierarchy ds= [ Cell ds ; Partial {entityType=Some ds.entity.etype };Global]

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
                                    let childrenEvaluated= let map= if(true) then Parallels.map else Seq.map
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
                                        else let lazyValue= (storeCache (name, newEnv.context) newEnv.bindigs )
                                             in (snd <| lazyValue.Force())




and cache= System.Collections.Concurrent. ConcurrentDictionary<string,Lazy<Set<string>*Value>>(100,10000) 

and storeCache (key,context) env :Lazy<Set<string>*Value>=     
    let cacheKey= match context with CellContext(ds,_)->String.Format("{0}.{1}.{2}", ds.entity.name, key, ds.date)
    let ds= match context with CellContext(dimensions,_)->dimensions
    let valueFactory= fun k->lazy(let exp=  List.tryPick (getExpFromStore key) (searchHierarchy ds) |> defaultArg <| Const 0.    
                                  let newEnv={bindigs=env ;context=context}
                                  in (Set.empty(*Set.ofList(List.map (fun (key,cxt)->buildContextKey key cxt) dependencies)*),eval newEnv exp))
    in cache.GetOrAdd(cacheKey,valueFactory)
                                                              
//and worker = spawnParallelWorker (fun f -> //printfn "doing some Work"
 //                                          f()) 10
(* open AgentSystem.LAgent
let agent =
   Agent.Start(fun inbox ->
     async { while true do
               let! msg = inbox.Receive()
               printfn "got message '%s'" msg } ) *)


