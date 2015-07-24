open Sys_lexer
open Sys_parser
open ParserUtils
open Printf

let parse_file s =
  try
    let ic = open_in s in
    let lb = Lexing.from_channel ic in
    system lexer lb
  with
  | Parsing.Parse_error -> printf "Syntax error on line number %d\n" !linenum
  | Failure s -> failwith (sprintf "Failure %s on line number %d\n" s !linenum)

let () =
  Arg.parse [ ]
    parse_file "Usage: main [fileinput]"