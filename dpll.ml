open Printf
open Set
open Glbdefs

let false_const = (Parent {valstr = "FALSE"; lchild = Empty (); rchild = Empty ()})
let true_const = (Parent {valstr = "TRUE"; lchild = Empty (); rchild = Empty ()})
let neg_false = (Parent {valstr = "~"; lchild = Empty (); rchild = false_const})
let neg_true = (Parent {valstr = "~"; lchild = Empty (); rchild = true_const})

let interpretation : (parseTree * bool) list ref = ref []

let clear_interpretation () = interpretation := []

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

let rec wff_to_string (f : parseTree) : string =
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
    sprintf "%s%s%s%s%s" (lside) (wff_to_string node.lchild) (node.valstr) (wff_to_string node.rchild) (rside)
    end

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
    (*printf("From cleanup:\n");
    FSetSet.iter (fun x -> (printf "clause:\n"; (FSet.iter display_wff x); printf "\n")) ((FSetSet.remove FSet.empty (FSetSet.of_list cleaned_clauses_list)));*)
    ((FSetSet.remove FSet.empty (FSetSet.of_list cleaned_clauses_list)), false)
  end

let rec bcp (clauses : FSetSet.t) : FSetSet.t =
  (*printf("\n\nBefore resolution:\n");
  FSetSet.iter (fun x -> (printf "clause:\n"; (FSet.iter display_wff x); printf "\n")) clauses;*)
  let is_unit_clause clause = (FSet.cardinal clause = 1) in
  (* Separate unit clauses from non-unit clauses *)
  let (unit_clauses, complex_clauses') = FSetSet.partition is_unit_clause clauses in
  let unit_clauses_cleaned = FSetSet.remove (FSet.singleton true_const) unit_clauses in
  (* If there are no unit clauses, then we can't resolve anything. *)
  if ((FSetSet.is_empty unit_clauses_cleaned)
  || (FSetSet.is_empty complex_clauses')) 
  then clauses else begin
    (* Choose an atom at random. *)
    let unit_clause = FSetSet.choose unit_clauses_cleaned in
    let atom = FSet.choose unit_clause in
    let no_atom clause = not (FSet.mem atom clause) in
    let (clauses', _) = FSetSet.partition no_atom clauses in
    let (complex_clauses, _) = FSetSet.partition no_atom complex_clauses' in
    (* We will resolve the negation of the chosen atom. *)
    let to_resolve = match atom with
      | Parent node -> begin match node.valstr with
        | "~" -> node.rchild
        | _ -> (Parent {valstr="~";lchild=Empty ();rchild=(Parent node)})
      end
      | Empty _ -> assert false
    in
    (* Find all complex clauses that have the resolvent. *)
    let contains_resolvent clause = (FSet.mem to_resolve clause) in
    let (has_resolvent, _) = FSetSet.partition contains_resolvent complex_clauses in
    (* If no clauses contain the resolvent, try picking a different unit clause *)
    let final = if (FSetSet.is_empty has_resolvent) then (FSetSet.add (FSet.singleton atom) (bcp (FSetSet.remove (FSet.singleton atom) clauses'))) else begin
      (* Otherwise, perform the resolution and return the updated clauses *)
      let has_resolvent_list = FSetSet.elements has_resolvent in
      let do_resolve clause = (FSet.remove to_resolve clause) in
      let resolved_list = List.map do_resolve has_resolvent_list in
      let resolved = FSetSet.of_list resolved_list in
      bcp (FSetSet.remove (FSet.singleton atom) (FSetSet.union (FSetSet.diff clauses' has_resolvent) resolved))
    end in
    (*printf("\n\nAfter resolution:\n");
    FSetSet.iter (fun x -> (printf "clause:\n"; (FSet.iter display_wff x); printf "\n")) final;*)
    final
  end

let choose_var (clauses : FSetSet.t) : parseTree =
  let clause = FSetSet.choose clauses in
  let subclause = FSet.choose clause in
  match subclause with
  | Parent node -> begin match node.valstr with
    | "~" -> node.rchild
    | _ -> Parent node
  end
  | Empty _ -> assert false

let substitute (clauses : FSetSet.t) (atom : parseTree) (value : bool) : FSetSet.t =
  let substitute_helper clause : FSet.t =
    let substitute_helper_helper subclause : parseTree =
      match subclause with
      | Parent node -> begin match node.valstr with
        | "~" -> if (node.rchild = atom) then begin
          if value then neg_true else neg_false
        end
        else Parent node
        | _ -> if ((Parent node) = atom) then begin
          if value then true_const else false_const
        end
        else Parent node
      end
      | Empty () -> assert false
    in
    let subclause_list = FSet.elements clause in
    let subclause_list' = List.map substitute_helper_helper subclause_list in
    FSet.of_list subclause_list'
  in
  let clause_list = FSetSet.elements clauses in
  let clause_list' = List.map substitute_helper clause_list in
  FSetSet.of_list clause_list'

let rec dpll (clauses : FSetSet.t) : (bool * (parseTree * bool) list)  =
  let dpll_t_val = ref false in
  let dpll_f_val = ref false in
  (*printf("From dpll:\n");
  FSetSet.iter (fun x -> (printf "clause:\n"; (FSet.iter display_wff x); printf "\n")) clauses;*)
  if ((clauses = (FSetSet.singleton (FSet.singleton true_const)))
  || (clauses = (FSetSet.singleton (FSet.singleton neg_false))))
  then (true, []) else begin
  let (cleaned, unsat) = cleanup clauses in
  if unsat then (false, []) else begin
    let clauses' = bcp cleaned in
    (*printf("From dpll (post cleaning):\n");
    FSetSet.iter (fun x -> (printf "clause:\n"; (FSet.iter display_wff x); printf "\n")) clauses';*)
    if (FSetSet.is_empty clauses') then (false, [])
    else if ((FSetSet.cardinal clauses') = 1) then begin
  	  let clause = (FSetSet.choose clauses') in
  	  if ((FSet.cardinal clause) = 1) then begin
  	    let atom = FSet.choose clause in
  	    if (atom = false_const) or (atom = neg_true) then (false, [])
  	    else if (atom = true_const) or (atom = neg_false) then (true, [])
  	    else begin 
          (*printf "\nSubstitute: %s |--> TRUE\n\n" (wff_to_string atom);*)
          let (dpll_true, _) = dpll (substitute clauses' atom true) in
          let _ = begin 
            if ((dpll_true) && (not (List.mem (atom,true) !interpretation))) then begin
              interpretation := !interpretation @ [atom, true];
              dpll_t_val := true;
            end else begin
              (*printf "\nSubstitute: %s |--> FALSE\n\n" (wff_to_string atom);*)
              let (dpll_false, _) = dpll (substitute clauses' atom false) in
              if (dpll_false) && (not (List.mem (atom,true) !interpretation)) then begin
                interpretation := !interpretation @ [atom, false];
                dpll_f_val := true;
              end
            end
          end in
          ((!dpll_t_val) || (!dpll_f_val)), !interpretation
        end
  	  end
      else begin
        let atom_choices = FSetSet.remove (FSet.singleton true_const) (FSetSet.remove (FSet.singleton false_const)
          (FSetSet.remove (FSet.singleton neg_true) (FSetSet.remove (FSet.singleton neg_true) clauses'))) in
        let atom = choose_var atom_choices in
        (*printf "\nSubstitute: %s |--> TRUE\n\n" (wff_to_string atom);*)
        let (dpll_true, _) = dpll (substitute clauses' atom true) in
          let _ = begin 
            if ((dpll_true) && (not (List.mem (atom,true) !interpretation))) then begin
              interpretation := !interpretation @ [atom, true];
              dpll_t_val := true;
            end else begin
              (*printf "\nSubstitute: %s |--> FALSE\n\n" (wff_to_string atom);*)
              let (dpll_false, _) = dpll (substitute clauses' atom false) in
              if (dpll_false) && (not (List.mem (atom,true) !interpretation)) then begin
                interpretation := !interpretation @ [atom, false];
                dpll_f_val := true;
              end
            end
          end in
          ((!dpll_t_val) || (!dpll_f_val)), !interpretation
      end
    end
    else begin
      let atom_choices = FSetSet.remove (FSet.singleton true_const) (FSetSet.remove (FSet.singleton false_const)
          (FSetSet.remove (FSet.singleton neg_true) (FSetSet.remove (FSet.singleton neg_true) clauses'))) in
      let atom = choose_var atom_choices in
      (*printf "\nSubstitute: %s |--> TRUE\n\n" (wff_to_string atom);*)
      let (dpll_true, _) = dpll (substitute clauses' atom true) in
          let _ = begin 
            if ((dpll_true) && (not (List.mem (atom,true) !interpretation))) then begin
              interpretation := !interpretation @ [atom, true];
              dpll_t_val := true;
            end else begin
              (*printf "\nSubstitute: %s |--> FALSE\n\n" (wff_to_string atom);*)
              let (dpll_false, _) = dpll (substitute clauses' atom false) in
              if (dpll_false) && (not (List.mem (atom,true) !interpretation)) then begin
                interpretation := !interpretation @ [atom, false];
                dpll_f_val := true;
              end
            end
          end in
          ((!dpll_t_val) || (!dpll_f_val)), !interpretation
    end
  end
  end