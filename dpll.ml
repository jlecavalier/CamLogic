open Printf
open Set
open Glbdefs

let cleanup (clauses : FSetSet.t) : FSetSet.t * bool =
	clauses

let bcp (clauses : FSetSet.t) : FSetSet.t = clauses

let choose_var (clauses : FSetSet.t) : parseTree =
  Parent {valstr="FALSE";lchild=Empty ();rchild=Empty ();}

let substitute (clauses : FSetSet.t) (atom : parseTree) (value : bool) : FSetSet.t =
  clauses

let rec dpll (clauses : FSetSet.t) : bool =
  let (cleaned, unsat) = cleanup clauses
  if unsat then false else begin
    let clauses' = bcp cleaned in
    if (FSetSet.is_empty clauses') then false
    else if ((FSetSet.cardinal clauses') = 1) then begin
  	  let clause = (FSetSet.choose clauses') in
  	  if ((FSet.cardinal clause) = 1) then begin
  	    let atom = FSet.choose clause in
  	    if (atom = (Parent {valstr="FALSE";lchild=Empty ();rchild=Empty ();})) then false
  	    else if (atom = (Parent {valstr="TRUE";lchild=Empty ();rchild=Empty ();})) then true
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