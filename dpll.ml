open Printf
open Set
open Glbdefs

let false_const = (Parent {valstr = "FALSE"; lchild = Empty (); rchild = Empty ()})
let true_const = (Parent {valstr = "TRUE"; lchild = Empty (); rchild = Empty ()})

let cleanup (clauses : FSetSet.t) : FSetSet.t * bool =
  if (FSetSet.mem (FSet.singleton false_const) clauses)
  then (clauses, true)
  else begin
    let cleanup_helper clause =
      let cleaned_clause = (FSet.remove true_const clause) in
      FSet.remove false_const cleaned_clause
    in
    ((FSetSet.iter cleanup_helper clauses), false)
  end

let bcp (clauses : FSetSet.t) : FSetSet.t = clauses

let choose_var (clauses : FSetSet.t) : parseTree =
  false_const

let substitute (clauses : FSetSet.t) (atom : parseTree) (value : bool) : FSetSet.t =
  clauses

let rec dpll (clauses : FSetSet.t) : bool =
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