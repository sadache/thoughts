// Signature file for parser generated by fsyacc
module FormulasParser
open Exp
type token = 
  | EOF
  | LPAREN
  | RPAREN
  | PLUS
  | MINUS
  | ASTER
  | SLASH
  | CHILDREN
  | REF of (System.String)
  | FLOAT of (System.Double)
type tokenId = 
    | TOKEN_EOF
    | TOKEN_LPAREN
    | TOKEN_RPAREN
    | TOKEN_PLUS
    | TOKEN_MINUS
    | TOKEN_ASTER
    | TOKEN_SLASH
    | TOKEN_CHILDREN
    | TOKEN_REF
    | TOKEN_FLOAT
    | TOKEN_end_of_input
    | TOKEN_error
type nonTerminalId = 
    | NONTERM__startstart
    | NONTERM_start
    | NONTERM_Prog
    | NONTERM_Expr
/// This function maps integers indexes to symbolic token ids
val tagOfToken: token -> int

/// This function maps integers indexes to symbolic token ids
val tokenTagToTokenId: int -> tokenId

/// This function maps production indexes returned in syntax errors to strings representing the non terminal that would be produced by that production
val prodIdxToNonTerminal: int -> nonTerminalId

/// This function gets the name of a token as a string
val token_to_string: token -> string
val start : (Microsoft.FSharp.Text.Lexing.LexBuffer<'cty> -> token) -> Microsoft.FSharp.Text.Lexing.LexBuffer<'cty> -> ( Exp ) 
