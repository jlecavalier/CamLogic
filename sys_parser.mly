%{
  (*open Printf*)
  open ParserUtils
%}

/* Propositions */
%token <string> LProposition
%token LTrue LFalse
/* Commands */
%token LEntail
/* Punctuation */
%token LOpenParen LCloseParen LSemiColon LComma
/* Operators */
%token LConditional LBiconditional LOr LAnd LNegation
/* End of the file */
%token LEnd

%left LConditional LBiconditional
%left LOr LAnd
%right LNegation

%start system
%type<unit> system

%%

/* The system parses commands and commands are ended
   with semicolons */
system: commands LEnd { }
;

commands: command LSemiColon { }
| command LSemiColon commands { }
;

/* commands are either wffs or a list of wffs followed
   by an entailment symbol followed by a single wff */

/* TODO:
   1. If the command is just a wff, tell the parser to
   see if the wff is satisfiable.
   2. If the command is an entailment command, tell the
   parser to check the validity of the argument. */
command: wff { store_formula $1; cnf $1; (*display_cnf_clauses ();*) if (eval_dpll ()) then Printf.printf("Satisfiable\n") else Printf.printf("Unsatisfiable\n") }
| wff_list LEntail wff { }
;

/* Lists of wffs are either empty, contain a single
   wff, or are a bunch of wffs separated by commas */
wff_list : { [] }
| wff {[$1]}
| wff LComma wff_list { $1::$3 }
;

/* wffs are either propositions alone,
   a wff with a unary operator applied,
   or a binary expression enclosed in parens. */
wff : LTrue { atomic_subform true }
| LFalse { atomic_subform false } 
| LProposition { proposition_subform $1 }
| LNegation wff { negation_subform $2 }

| LOpenParen wff LConditional wff LCloseParen { binary_subform $2 $4 0 }
| wff LConditional wff { binary_subform $1 $3 0 }

| LOpenParen wff LBiconditional wff LCloseParen { binary_subform $2 $4 1 }
| wff LBiconditional wff { binary_subform $1 $3 1 }

| LOpenParen wff LAnd wff LCloseParen { binary_subform $2 $4 2 }
| wff LAnd wff { binary_subform $1 $3 2 }

| LOpenParen wff LOr wff LCloseParen { binary_subform $2 $4 3 }
| wff LOr wff { binary_subform $1 $3 3 }
;