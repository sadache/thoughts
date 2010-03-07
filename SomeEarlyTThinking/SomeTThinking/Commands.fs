module Commands
open Exp
type Commands= Unit of Name * AttachementLevel * Exp
               //|Value of Exp
               |Return of Name * AttachementLevel

let compile = Lexing.LexBuffer<char>.FromString >> FormulasParser.start FormulasLexer.tokenize