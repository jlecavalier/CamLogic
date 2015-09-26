open Printf
open Set
open Glbdefs

let false_const = (Parent {valstr = "FALSE"; lchild = Empty (); rchild = Empty ()})
let true_const = (Parent {valstr = "TRUE"; lchild = Empty (); rchild = Empty ()})
let neg_false = (Parent {valstr = "~"; lchild = Empty (); rchild = false_const})
let neg_true = (Parent {valstr = "~"; lchild = Empty (); rchild = true_const})

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

let cleanup (clauses : FSetSet.t) : FSetSet.t * bool =
  if ((FSetSet.mem (FSet.singleton false_const) clauses)
  || (FSetSet.mem (FSet.singleton neg_true) clauses))
  then (clauses, true)
  else begin
    let cleanup_helper clause =
      if ((FSet.mem true_const clause) || (FSet.mem neg_false clause)) then (FSet.singleton true_const)
      else begin
        let cleaned_clause = (FSet.remove neg_true clause) in
        FSet.remove false_const cleaned_clause
      end
    in
    let clauses_list = FSetSet.elements clauses in
    let cleaned_clauses_list = List.map cleanup_helper clauses_list in
    printf("From cleanup:\n");
    FSetSet.iter (fun x -> (printf "clause:\n"; (FSet.iter display_wff x); printf "\n")) (FSetSet.remove FSet.empty (FSetSet.of_list cleaned_clauses_list));
    ((FSetSet.remove FSet.empty (FSetSet.of_list cleaned_clauses_list)), true)
  end

let bcp (clauses : FSetSet.t) : FSetSet.t = clauses

let choose_var (clauses : FSetSet.t) : parseTree =
  false_const

let substitute (clauses : FSetSet.t) (atom : parseTree) (value : bool) : FSetSet.t =
  clauses

let rec dpll (clauses : FSetSet.t) : bool =
  if ((clauses = (FSetSet.singleton (FSet.singleton true_const)))
  || (clauses = (FSetSet.singleton (FSet.singleton neg_false))))
  then true else begin
  let (cleaned, unsat) = cleanup clauses in
  if unsat then false else begin
    let clauses' = bcp cleaned in
    if (FSetSet.is_empty clauses') then false
    else if ((FSetSet.cardinal clauses') = 1) then begin
  	  let clause = (FSetSet.choose clauses') in
  	  if ((FSet.cardinal clause) = 1) then begin
  	    let atom = FSet.choose clause in
  	    if (atom = false_const) then false
  	    else if (atom = true_const) then true
  	    else (dpll (substitute clauses' atom true)) || (dpll (substitute clauses' atom false))
  	  end
      else begin
        let atom = choose_var clauses' in
        (dpll (substitute clauses' atom true)) || (dpll (substitute clauses' atom false))
      end
    end
    else begin
      let atom = choose_var clauses in
      (dpll (substitute clauses' atom true)) || (dpll (substitute clauses' atom false))
    end
  end
  end