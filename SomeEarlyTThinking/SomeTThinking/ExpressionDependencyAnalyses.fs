module ExpressionDependencyAnalyses
open Exp

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