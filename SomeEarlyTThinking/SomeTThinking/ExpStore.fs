module ExpStore
open Exp
open System
open System.Collections.Generic

let buildContextKey rawkey  =
             function Cell(ds) -> String.Format("{0}.{1}.{2}", ds.entity.name, rawkey, ds.date)
                      |Partial ods -> 
                        String.Format("{0}.{1}", defaultArg ods.entityType "_", rawkey)
                      | Global -> rawkey
let calcStore= new Dictionary<string,Exp>()
let getCalcFromStore qualifiedKey= if calcStore.ContainsKey qualifiedKey then Some calcStore.[qualifiedKey] else None
let qualifiedKey (context,key)= buildContextKey key context
