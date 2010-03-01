// Implementation file for parser generated by fsyacc
#nowarn "64";; // turn off warnings that type variables used in production annotations are instantiated to concrete type
open Microsoft.FSharp.Text.Lexing
open Microsoft.FSharp.Text.Parsing.ParseHelpers
# 1 "D:\EveningWork\t-thoughts-with-redis\SomeEarlyTThinking\SomeTThinking\FormulasParser.fsy"


open Exp


# 11 "D:\EveningWork\t-thoughts-with-redis\SomeEarlyTThinking\SomeTThinking\FormulasParser.fs"
// This type is the type of tokens accepted by the parser
type token = 
  | EOF
  | PLUS
  | MINUS
  | ASTER
  | SLASH
  | FLOAT of (System.Double)
  | INT32 of (System.Int32)
// This type is used to give symbolic names to token indexes, useful for error messages
type tokenId = 
    | TOKEN_EOF
    | TOKEN_PLUS
    | TOKEN_MINUS
    | TOKEN_ASTER
    | TOKEN_SLASH
    | TOKEN_FLOAT
    | TOKEN_INT32
    | TOKEN_end_of_input
    | TOKEN_error
// This type is used to give symbolic names to token indexes, useful for error messages
type nonTerminalId = 
    | NONTERM__startstart
    | NONTERM_start
    | NONTERM_Prog
    | NONTERM_Expr
    | NONTERM_Term
    | NONTERM_Factor

// This function maps tokens to integers indexes
let tagOfToken (t:token) = 
  match t with
  | EOF  -> 0 
  | PLUS  -> 1 
  | MINUS  -> 2 
  | ASTER  -> 3 
  | SLASH  -> 4 
  | FLOAT _ -> 5 
  | INT32 _ -> 6 

// This function maps integers indexes to symbolic token ids
let tokenTagToTokenId (tokenIdx:int) = 
  match tokenIdx with
  | 0 -> TOKEN_EOF 
  | 1 -> TOKEN_PLUS 
  | 2 -> TOKEN_MINUS 
  | 3 -> TOKEN_ASTER 
  | 4 -> TOKEN_SLASH 
  | 5 -> TOKEN_FLOAT 
  | 6 -> TOKEN_INT32 
  | 9 -> TOKEN_end_of_input
  | 7 -> TOKEN_error
  | _ -> failwith "tokenTagToTokenId: bad token"

/// This function maps production indexes returned in syntax errors to strings representing the non terminal that would be produced by that production
let prodIdxToNonTerminal (prodIdx:int) = 
  match prodIdx with
    | 0 -> NONTERM__startstart 
    | 1 -> NONTERM_start 
    | 2 -> NONTERM_Prog 
    | 3 -> NONTERM_Expr 
    | 4 -> NONTERM_Expr 
    | 5 -> NONTERM_Expr 
    | 6 -> NONTERM_Term 
    | 7 -> NONTERM_Term 
    | 8 -> NONTERM_Term 
    | 9 -> NONTERM_Factor 
    | 10 -> NONTERM_Factor 
    | _ -> failwith "prodIdxToNonTerminal: bad production index"

let _fsyacc_endOfInputTag = 9 
let _fsyacc_tagOfErrorTerminal = 7

// This function gets the name of a token as a string
let token_to_string (t:token) = 
  match t with 
  | EOF  -> "EOF" 
  | PLUS  -> "PLUS" 
  | MINUS  -> "MINUS" 
  | ASTER  -> "ASTER" 
  | SLASH  -> "SLASH" 
  | FLOAT _ -> "FLOAT" 
  | INT32 _ -> "INT32" 

// This function gets the data carried by a token as an object
let _fsyacc_dataOfToken (t:token) = 
  match t with 
  | EOF  -> (null : System.Object) 
  | PLUS  -> (null : System.Object) 
  | MINUS  -> (null : System.Object) 
  | ASTER  -> (null : System.Object) 
  | SLASH  -> (null : System.Object) 
  | FLOAT _fsyacc_x -> Microsoft.FSharp.Core.Operators.box _fsyacc_x 
  | INT32 _fsyacc_x -> Microsoft.FSharp.Core.Operators.box _fsyacc_x 
let _fsyacc_gotos = [| 0us; 65535us; 1us; 65535us; 0us; 1us; 1us; 65535us; 0us; 2us; 1us; 65535us; 0us; 3us; 3us; 65535us; 0us; 9us; 5us; 6us; 7us; 8us; 5us; 65535us; 0us; 14us; 5us; 14us; 7us; 14us; 10us; 11us; 12us; 13us; |]
let _fsyacc_sparseGotoTableRowOffsets = [|0us; 1us; 3us; 5us; 7us; 11us; |]
let _fsyacc_stateToProdIdxsTableElements = [| 1us; 0us; 1us; 0us; 1us; 1us; 3us; 2us; 3us; 4us; 1us; 2us; 1us; 3us; 3us; 3us; 6us; 7us; 1us; 4us; 3us; 4us; 6us; 7us; 3us; 5us; 6us; 7us; 1us; 6us; 1us; 6us; 1us; 7us; 1us; 7us; 1us; 8us; 1us; 9us; 1us; 10us; |]
let _fsyacc_stateToProdIdxsTableRowOffsets = [|0us; 2us; 4us; 6us; 10us; 12us; 14us; 18us; 20us; 24us; 28us; 30us; 32us; 34us; 36us; 38us; 40us; |]
let _fsyacc_action_rows = 17
let _fsyacc_actionTableElements = [|2us; 32768us; 5us; 15us; 6us; 16us; 0us; 49152us; 0us; 16385us; 3us; 32768us; 0us; 4us; 1us; 5us; 2us; 7us; 0us; 16386us; 2us; 32768us; 5us; 15us; 6us; 16us; 2us; 16387us; 3us; 10us; 4us; 12us; 2us; 32768us; 5us; 15us; 6us; 16us; 2us; 16388us; 3us; 10us; 4us; 12us; 2us; 16389us; 3us; 10us; 4us; 12us; 2us; 32768us; 5us; 15us; 6us; 16us; 0us; 16390us; 2us; 32768us; 5us; 15us; 6us; 16us; 0us; 16391us; 0us; 16392us; 0us; 16393us; 0us; 16394us; |]
let _fsyacc_actionTableRowOffsets = [|0us; 3us; 4us; 5us; 9us; 10us; 13us; 16us; 19us; 22us; 25us; 28us; 29us; 32us; 33us; 34us; 35us; |]
let _fsyacc_reductionSymbolCounts = [|1us; 1us; 2us; 3us; 3us; 1us; 3us; 3us; 1us; 1us; 1us; |]
let _fsyacc_productionToNonTerminalTable = [|0us; 1us; 2us; 3us; 3us; 3us; 4us; 4us; 4us; 5us; 5us; |]
let _fsyacc_immediateActions = [|65535us; 49152us; 16385us; 65535us; 16386us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 16390us; 65535us; 16391us; 16392us; 16393us; 16394us; |]
let _fsyacc_reductions ()  =    [| 
# 117 "D:\EveningWork\t-thoughts-with-redis\SomeEarlyTThinking\SomeTThinking\FormulasParser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data :  Ast.equation )) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
                      raise (Microsoft.FSharp.Text.Parsing.Accept(Microsoft.FSharp.Core.Operators.box _1))
                   )
                 : '_startstart));
# 126 "D:\EveningWork\t-thoughts-with-redis\SomeEarlyTThinking\SomeTThinking\FormulasParser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : 'Prog)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 26 "D:\EveningWork\t-thoughts-with-redis\SomeEarlyTThinking\SomeTThinking\FormulasParser.fsy"
                                   Equation(_1) 
                   )
# 26 "D:\EveningWork\t-thoughts-with-redis\SomeEarlyTThinking\SomeTThinking\FormulasParser.fsy"
                 :  Ast.equation ));
# 137 "D:\EveningWork\t-thoughts-with-redis\SomeEarlyTThinking\SomeTThinking\FormulasParser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : 'Expr)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 29 "D:\EveningWork\t-thoughts-with-redis\SomeEarlyTThinking\SomeTThinking\FormulasParser.fsy"
                                       _1 
                   )
# 29 "D:\EveningWork\t-thoughts-with-redis\SomeEarlyTThinking\SomeTThinking\FormulasParser.fsy"
                 : 'Prog));
# 148 "D:\EveningWork\t-thoughts-with-redis\SomeEarlyTThinking\SomeTThinking\FormulasParser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : 'Expr)) in
            let _3 = (let data = parseState.GetInput(3) in (Microsoft.FSharp.Core.Operators.unbox data : 'Term)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 32 "D:\EveningWork\t-thoughts-with-redis\SomeEarlyTThinking\SomeTThinking\FormulasParser.fsy"
                                              BinaryExp(Plus,_1, _3) 
                   )
# 32 "D:\EveningWork\t-thoughts-with-redis\SomeEarlyTThinking\SomeTThinking\FormulasParser.fsy"
                 : 'Expr));
# 160 "D:\EveningWork\t-thoughts-with-redis\SomeEarlyTThinking\SomeTThinking\FormulasParser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : 'Expr)) in
            let _3 = (let data = parseState.GetInput(3) in (Microsoft.FSharp.Core.Operators.unbox data : 'Term)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 33 "D:\EveningWork\t-thoughts-with-redis\SomeEarlyTThinking\SomeTThinking\FormulasParser.fsy"
                                                BinaryExp(Minus,_1, _3) 
                   )
# 33 "D:\EveningWork\t-thoughts-with-redis\SomeEarlyTThinking\SomeTThinking\FormulasParser.fsy"
                 : 'Expr));
# 172 "D:\EveningWork\t-thoughts-with-redis\SomeEarlyTThinking\SomeTThinking\FormulasParser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : 'Term)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 34 "D:\EveningWork\t-thoughts-with-redis\SomeEarlyTThinking\SomeTThinking\FormulasParser.fsy"
                                       Term(_1) 
                   )
# 34 "D:\EveningWork\t-thoughts-with-redis\SomeEarlyTThinking\SomeTThinking\FormulasParser.fsy"
                 : 'Expr));
# 183 "D:\EveningWork\t-thoughts-with-redis\SomeEarlyTThinking\SomeTThinking\FormulasParser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : 'Term)) in
            let _3 = (let data = parseState.GetInput(3) in (Microsoft.FSharp.Core.Operators.unbox data : 'Factor)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 37 "D:\EveningWork\t-thoughts-with-redis\SomeEarlyTThinking\SomeTThinking\FormulasParser.fsy"
                                               BinaryExp(Times,_1, _3) 
                   )
# 37 "D:\EveningWork\t-thoughts-with-redis\SomeEarlyTThinking\SomeTThinking\FormulasParser.fsy"
                 : 'Term));
# 195 "D:\EveningWork\t-thoughts-with-redis\SomeEarlyTThinking\SomeTThinking\FormulasParser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : 'Term)) in
            let _3 = (let data = parseState.GetInput(3) in (Microsoft.FSharp.Core.Operators.unbox data : 'Factor)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 38 "D:\EveningWork\t-thoughts-with-redis\SomeEarlyTThinking\SomeTThinking\FormulasParser.fsy"
                                               BinaryExp(Divide,_1, _3) 
                   )
# 38 "D:\EveningWork\t-thoughts-with-redis\SomeEarlyTThinking\SomeTThinking\FormulasParser.fsy"
                 : 'Term));
# 207 "D:\EveningWork\t-thoughts-with-redis\SomeEarlyTThinking\SomeTThinking\FormulasParser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : 'Factor)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 39 "D:\EveningWork\t-thoughts-with-redis\SomeEarlyTThinking\SomeTThinking\FormulasParser.fsy"
                                     Factor(_1) 
                   )
# 39 "D:\EveningWork\t-thoughts-with-redis\SomeEarlyTThinking\SomeTThinking\FormulasParser.fsy"
                 : 'Term));
# 218 "D:\EveningWork\t-thoughts-with-redis\SomeEarlyTThinking\SomeTThinking\FormulasParser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : System.Double)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 42 "D:\EveningWork\t-thoughts-with-redis\SomeEarlyTThinking\SomeTThinking\FormulasParser.fsy"
                                     Float(_1) 
                   )
# 42 "D:\EveningWork\t-thoughts-with-redis\SomeEarlyTThinking\SomeTThinking\FormulasParser.fsy"
                 : 'Factor));
# 229 "D:\EveningWork\t-thoughts-with-redis\SomeEarlyTThinking\SomeTThinking\FormulasParser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : System.Int32)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 43 "D:\EveningWork\t-thoughts-with-redis\SomeEarlyTThinking\SomeTThinking\FormulasParser.fsy"
                                     Integer(_1) 
                   )
# 43 "D:\EveningWork\t-thoughts-with-redis\SomeEarlyTThinking\SomeTThinking\FormulasParser.fsy"
                 : 'Factor));
|]
# 241 "D:\EveningWork\t-thoughts-with-redis\SomeEarlyTThinking\SomeTThinking\FormulasParser.fs"
let tables () : Microsoft.FSharp.Text.Parsing.Tables<_> = 
  { reductions= _fsyacc_reductions ();
    endOfInputTag = _fsyacc_endOfInputTag;
    tagOfToken = tagOfToken;
    dataOfToken = _fsyacc_dataOfToken; 
    actionTableElements = _fsyacc_actionTableElements;
    actionTableRowOffsets = _fsyacc_actionTableRowOffsets;
    stateToProdIdxsTableElements = _fsyacc_stateToProdIdxsTableElements;
    stateToProdIdxsTableRowOffsets = _fsyacc_stateToProdIdxsTableRowOffsets;
    reductionSymbolCounts = _fsyacc_reductionSymbolCounts;
    immediateActions = _fsyacc_immediateActions;
    gotos = _fsyacc_gotos;
    sparseGotoTableRowOffsets = _fsyacc_sparseGotoTableRowOffsets;
    tagOfErrorTerminal = _fsyacc_tagOfErrorTerminal;
    parseError = (fun (ctxt:Microsoft.FSharp.Text.Parsing.ParseErrorContext<_>) -> 
                              match parse_error_rich with 
                              | Some f -> f ctxt
                              | None -> parse_error ctxt.Message);
    numTerminals = 10;
    productionToNonTerminalTable = _fsyacc_productionToNonTerminalTable  }
let engine lexer lexbuf startState = (tables ()).Interpret(lexer, lexbuf, startState)
let start lexer lexbuf :  Ast.equation  =
    Microsoft.FSharp.Core.Operators.unbox ((tables ()).Interpret(lexer, lexbuf, 0))
