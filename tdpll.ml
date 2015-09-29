open Glbdefs
open Dpll
open Printf

let f1 = (Parent {valstr="A";lchild=Empty ();rchild=Empty ()})
let f2 = (Parent {valstr="~";lchild=Empty ();rchild=f1})
let f3 = (Parent {valstr="B";lchild=Empty ();rchild=Empty ()})
let f4 = (Parent {valstr="~";lchild=Empty ();rchild=f3})
let f5 = (Parent {valstr="C";lchild=Empty ();rchild=Empty ()})
let f6 = (Parent {valstr="~";lchild=Empty ();rchild=f5})

let clauses = ref FSetSet.empty
let clause = ref FSet.empty

let display_cnf_clauses () : unit = FSetSet.iter (fun x -> (printf "clause:\n"; (FSet.iter display_wff x); printf "\n")) !clauses

let display_wff (f : parseTree) : unit =
  (*printf "The formula at line %d is:\n\n" !linenum;*)
  let rec display_wff_helper (f : parseTree) : string =
    match f with 
    | Empty () -> sprintf ""
    | Parent node -> begin
      let (lside, rside) =
        match node.valstr with
        | "=>" -> ("(",")")
        | "<=>" -> ("(",")")
        | "&" -> ("(",")")
        | "|" -> ("(",")")
        | _ -> ("","")
      in
      sprintf "%s%s%s%s%s" (lside) (display_wff_helper node.lchild) (node.valstr) (display_wff_helper node.rchild) (rside)
      end
  in
  printf "%s\n" (display_wff_helper f)