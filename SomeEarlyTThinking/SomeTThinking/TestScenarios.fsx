#r "Dependencies/ConsoleApplication14.dll"
#r "Dependencies/FSharp.PowerPack.dll"
#load "../Agents/AgentSystem.fs"
#load "UsefulStuff.fs"
#load "Exp.fs"
#load "ExpStore.fs"
#load "Module1.fs"
#load "FormulasParser.fs"
#load "FormulasLexer.fs"


// Samples
open System
open ConsoleApplication14
open Exp
open Module1
open FormulasParser
open FormulasLexer

let lexbuff = Lexing.LexBuffer<char>.FromString " (\ooo/ 1.2 * (1 + &RENT)) "
let equation = FormulasParser.start FormulasLexer.tokenize lexbuff

let bindings0= Map.empty
let env0With context= {bindigs= bindings0; context=context}

let entityGraph = EntityDependencyGraph()

let cellByName entityname (ddate:int) = 
                                        let entity = match entityGraph.Item(entityname) with (e,_) -> e
                                        in Cell {entity =entity ;date= ddate}

let cellCtx entity (date:int) = CellContext({entity =entity ;date= date}, fun _ -> entityGraph)

let define= calcStore.Add 


// IDX_CHG	[IDX_CHG (-1)]
calcStore.Add (qualifiedKey (Global, "IDX_CHG") ,(Ref("IDX_CHG", previousMonth)))

// IDX_RENT	[IDX_RENT (-1)]
calcStore.Add (qualifiedKey (Global, "IDX_RENT") ,(Ref("IDX_RENT", previousMonth)))

// CARRYFWD	[CARRYFWD (-1)]+[RESULT_AT]-[DIVID_PAY]
calcStore.Add (qualifiedKey (Global, "CARRYFWD") ,(Ref("CARRYFWD", previousMonth) .+. local "RESULT_AT" .-. local "DIVID_PAY"))

// DIVID_PAY	IIF([CARRYFWD (-1)]+[RESULT_AT]>0,fMIN([CARRYFWD (-1)]+[RESULT_AT],[DIVID_REQ]),0)
calcStore.Add (qualifiedKey (Global, "DIVID_PAY") ,(If
                                    ( (Ref("CARRYFWD", previousMonth) .+. local "RESULT_AT")  .>. Const(0.), 
                                        minE (Ref("CARRYFWD", previousMonth) .+. local "RESULT_AT") (local "DIVID_REQ"), 
                                        Const(0.))))

// RESULT_AT	IIF({Month}=12,[RESULT_BT_YTD]+[TAX_AMOUNT],0)
calcStore.Add (qualifiedKey  (Global ,"RESULT_AT"), (If (Context Month .=. Const(12.), local "RESULT_BT_YTD" .+. local "TAX_AMOUNT", Const(0.))))

// TAX_AMOUNT	-[TAX_BASE]*[TAX_RATE]
calcStore.Add (qualifiedKey  (Global ,"TAX_AMOUNT"), (Const(-1.) .*. local "TAX_BASE" .*. local "TAX_RATE") )

// TAX_RATE	[TAX_RATE (-1)]
calcStore.Add (qualifiedKey  ( Global, "TAX_RATE"), (Ref("TAX_RATE", previousMonth)))

// TAX_BASE	IIF({Month}=12,fMAX(0,fMIN([RESULT_BT_YTD],[RESULT_BT_YTD]+[CARRYFWD (-1)])),0)
calcStore.Add (qualifiedKey  (Global, "TAX_BASE"), (If(Context Month .=. Const(12.), 
                                                            maxE (Const(0.)) (minE (local "RESULT_BT_YTD") (local "RESULT_BT_YTD" .+. Ref("CARRYFWD", previousMonth))), 
                                                            Const(0.))))

// RESULT_BT_YTD	IIF({Month}=1,[RESULT_BT],[RESULT_BT]+[RESULT_BT_YTD (-1)])
calcStore.Add (qualifiedKey  ( Global ,"RESULT_BT_YTD"), (If(Context Month .=. Const(1.), local "RESULT_BT", local "RESULT_BT" .+. Ref("RESULT_BT_YTD", previousMonth))))

// RESULT_BT	[RESULT_OP]+[RESULT_FIN]+[RESULT_EXTRA]
calcStore.Add (qualifiedKey  ( Global, "RESULT_BT"), (local "RESULT_OP" .+. local "RESULT_FIN" .+. local "RESULT_EXTRA"))

// RESULT_EXTRA	[EXTRAINC]+[EXTRAFEE]
calcStore.Add (qualifiedKey  ( Global, "RESULT_EXTRA"), (local "EXTRAINC" .+. local "EXTRAFEE")) 

// RESULT_FIN	[FININC]+[FINCOST]
calcStore.Add (qualifiedKey  ( Global ,"RESULT_FIN"), (local "FININC" .+. local "FINCOST"))

// FININC	[DIVID_PAY ,(childsw)]
calcStore.Add (qualifiedKey  (Global ,"FININC"), (Children(Sum, local "DIVID_PAY")))
calcStore.Add (qualifiedKey  (Partial{entityType=Some "Company"}, "FININC"), (Const(0.)))

// AMTZ
calcStore.Add (qualifiedKey  ( Global, "AMTZ"), (Const(0.)))

// RESULT_OP	[RENTS]+[CHARGES]+[WORKS]+[AMTZ]
calcStore.Add (qualifiedKey  (Global ,"RESULT_OP"), (local "RENTS" .+. local "CHARGES" .+. local "WORKS" .+. local "AMTZ"))
calcStore.Add (qualifiedKey  ( Partial{entityType=Some "Holding"}, "RESULT_OP"), (Const(0.)))

// WORKS	[WORKS (-12)]*(1+[IDX_CHG])
calcStore.Add (qualifiedKey  ( Global, "WORKS"), ((Ref("WORKS", previousYear)) .*. (local "IDX_CHG" .+. Const(1.)) ))

calcStore.Add (qualifiedKey  ( Partial{entityType=Some "Holding"}, "WORKS"), (Const(0.)))

// CHARGES	[CHARGES (-1)]
calcStore.Add (qualifiedKey  ( Global, "CHARGES"), (Ref("CHARGES", previousYear)))
calcStore.Add (qualifiedKey  ( Partial{entityType=Some "Holding"}, "CHARGES"), (Const(0.)))

// RENTS	[RENTS (-12)]*(1+[IDX_RENT])
calcStore.Add (qualifiedKey  ( Global, "RENTS" ),((Ref("RENTS", previousYear)) .*. (local "IDX_RENT" .+. Const(1.)) )) 
calcStore.Add (qualifiedKey  ( Partial{entityType=Some"Holding"}, "RENTS"), (Const(0.)))


//
let entityWithName n = match entityGraph.Item(n) with (e, r) -> e

let testData = new GetTestData("TestsData/Allianz")
let i_d:int->double = Convert.ToDouble
let f_d:float->double = Convert.ToDouble

let _entity (e:ImportedEntity) = {name=e.Code;etype=e.EntityType;dateBounds=(monthsAway e.Begins,monthsAway e.Ends)}

Seq.iter (fun (r:ImportedEntityLinks) -> let e = _entity(r.Entity)
                                         let ownership = 
                                            Seq.toList (Seq.map(fun (o:ImportedOwnership) -> _entity(o.Entity), (f_d(o.Hold):OwnershipRatio)) r.Owns)
                                         in entityGraph.Add(e.name,(e, Owns(ownership)))
                                         ) testData.EntitiesLinks


Seq.iter (fun (c:ImportedRow) -> 
                  calcStore.Add(qualifiedKey  (cellByName (c.Entity.Code)  (monthsAway c.Date), c.Nature), (Const(f_d(c.Value)))))  testData.Cells


cache.Clear()
//
let ctx = (cellCtx (entityWithName "XF_DE001") 120)
//let dps=entityGraph
let exp = ((local "RESULT_FIN"))

let sw = new System.Diagnostics.Stopwatch()
sw.Start()
let res = eval (env0With ctx) exp
//let store=calcStore
sw.Stop()
let elaplsed=sw.ElapsedMilliseconds

