module Module1

open System
open System.Collections.Generic

type Name= string
type Year = int

type CellContext = Dim2 of Name * Year

type CellContextTrans = CellContext -> CellContext

type Exp = Const of double
           |Binding of Name * CellContextTrans
           |Plus of Exp * Exp
           |Fun of Name * Exp
           |App of Exp * Exp

type CalcMap = Dictionary<Name, Exp>

type Env= Map<Name,Value>

and Value= DoubleVal of double
           | FunVal of Env * Name * Exp

let funN args exp= let rec doArgs = 
                       function (head::[])-> Fun(head,exp)
                                |(h::tail) -> Fun(h,doArgs tail)
                                |_ -> raise (Exception())
                    in doArgs args      

let appN f exps= let rec doArgs=
                     function(head::[])-> App(f, head)
                            |(h::tail) -> App(doArgs tail, h)
                            |_ -> raise (Exception())

                  in doArgs exps

let rec eval env (ctx:CellContext) (cmap:CalcMap) =
    let applyContextTrans (contextTrans:CellContextTrans) c = contextTrans c
    let buildQueryContext n (Dim2(entityname, year)) =  String.Format("{0}.{1}.{2}", entityname, n, year)
    let gotogetExp name = if cmap.ContainsKey(name) then Some(cmap.Item(name)) else None

    let gotogetValue name trans (env:Env) = let newCtx = trans ctx
                                            in 
                                            let ctxQuery = buildQueryContext name newCtx
                                            in match(gotogetExp ctxQuery) with
                                                   Some(e) -> eval env newCtx cmap e
                                                    | _ -> raise (Exception())

    in function Const (d)-> DoubleVal d
              | Binding (name, trans) -> match(env.TryFind(name)) with
                                            Some(v) -> v
                                            | None -> gotogetValue name trans env
              | Plus(e1,e2)-> match((eval env ctx cmap e1,eval env ctx cmap e2)) with
                                    (DoubleVal(d1),DoubleVal(d2))-> DoubleVal (d1+d2)
                                    |_ -> raise(Exception())

              | Fun(name,e)-> FunVal(env,name,e)
              | App (ef,e1) -> match(eval env ctx cmap ef ) with
                                FunVal(env,name,e)->
                                    let newEnv=Map.add name (eval env ctx cmap e1) env
                                    in eval newEnv ctx cmap e
                                |_ -> raise (Exception())

let nulCellContextTrans (c:CellContext) = c

let previousYearTrans (Dim2(entityname, year)) =Dim2(entityname, year-1)

let var a = Binding(a, nulCellContextTrans)

let (<+>) a b = Plus (a,b)
let ($) = appN

//

let calcMap = CalcMap()
let ctx = Dim2("entity1",2010)
let env0:Env= Map.empty
//

let rent = Const 100.0
let charges = Const -10.0
let works = Const -10.0
let amortization = Const -10.0

let operationalResult = eval env0 ctx calcMap (appN (funN ["a"; "b"; "c"; "d"] (Plus((Plus((Plus(var "a", var "b")), var "c")), var "d"))) [rent; charges; works; amortization])
let operationalResult2 = funN ["a"; "b"; "c"; "d"] (var "a" <+> var "b" <+> var "c" <+> var "d") $ [rent; charges; works; amortization]
let operationalResult2' = eval env0 ctx calcMap operationalResult2

//

let operationalResult3 = funN ["a"; "b"; "c"; "d"] (var "a" <+> var "b" <+> var "c" <+> var "d") $ [var "rent"; var "charges"; var "works"; var "amortization"]
calcMap.Add("entity1.rent.2010", Const 100.0)
calcMap.Add("entity1.charges.2010", Const -10.0)
calcMap.Add("entity1.works.2010", Const -10.0)
calcMap.Add("entity1.amortization.2010", Const -20.0)
calcMap.Add("entity1.operationalResult.2010", operationalResult3)

let operationalResult3' = eval env0 ctx calcMap (var "operationalResult") // 60

let natureXpreviousYearTrans = Binding("natureX", previousYearTrans)

calcMap.Add("entity1.natureX.2010", funN ["a"; "b"; "c"; "d"] (var "a" <+> var "b" <+> var "c" <+> var "d") $ [var "rent"; var "charges"; var "works"; var "amortization"])
calcMap.Add("entity1.natureX.2011", natureXpreviousYearTrans)
calcMap.Add("entity1.natureX.2012", natureXpreviousYearTrans)

let ctx' = Dim2("entity1", 2012)

let natureXFor2012 = eval env0 ctx' calcMap (var "natureX") // 60 