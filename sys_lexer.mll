{
  open ParserUtils
  open Sys_parser
  open Printf
}

let whitespace = [' ' '\t']

rule lexer = parse
  | "|=" { LEntail }
  | "entails" { LEntail }
  | whitespace { lexer lexbuf }
  | '\n' { incr linenum; lexer lexbuf }
  | "(" { LOpenParen }
  | ")" { LCloseParen }
  | ";" { LSemiColon }
  | "," { LComma }
  | "=>" { LConditional }
  | "<=>" { LBiconditional }
  | "|" { LOr }
  | "&" { LAnd }
  | "~" { LNegation }
  | "true" { LTrue }
  | "false" { LFalse }
  | "TRUE" { LTrue }
  | "FALSE" { LFalse }
  | eof { LEnd }
  | ['A'-'Z'] { LProposition (Lexing.lexeme lexbuf) }
  | _ as x { failwith (sprintf "Error: I don't know what %c means, but I saw it on line %d" x !linenum) }