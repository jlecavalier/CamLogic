open Printf
open Set
(* Propositions are given as strings, but we identify
   each one with a number instead of using the string. *)
type propositionData = {
  id: int;
  name: string
}

(* Formulas are represented as parse trees. *)
type parseTree =
  | Empty of unit
  | Parent of parseNode

(* Each node contains a string and two children *)
and parseNode = {
  valstr: string;
  lchild: parseTree;
  rchild: parseTree
}

(* We keep a set of subformulae for each formula, so that we
   can easily convert to CNF. *)
module FSet = Set.Make(
  struct
    let compare =
  Pervasives.compare
    type t = parseTree
  end)

let propositionList : propositionData list ref = ref [] (* The list of propositions we are working with *)
let nPropositions : int ref = ref 0 (* number of propositions we're working with *)
let linenum : int ref  = ref 1 (* The line number we're parsing. *)
let subformulae = ref FSet.empty (* A set of subformulae *)
let fhtbl = Hashtbl.create 5 (* Maps formulae to their set of subformulae *)

(* Notify the user of errors. *)
let warn str = fprintf stderr "@L%d: Warning: %s \n" !linenum str
let fatal str = fprintf stderr "@L%d: Fatal: %s \n" !linenum str; assert false

(* Prints a formula to the terminal screen *)
let display_wff (f : parseTree) : unit =
  (*printf "The formula at line %d is:\n\n" !linenum;*)
  printf "\n\n";
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
  printf "%s\n\n" (display_wff_helper f)

(* Either TRUE or FALSE *)
let atomic_subform (b : bool) : parseTree =
  let str = if (b) then "TRUE" else "FALSE" in
  let subform =
  { valstr = str;
    lchild = Empty ();
    rchild = Empty (); }
  in
  subformulae := (FSet.add (Parent subform) !subformulae);
  (*display_wff (Parent subform);*)
  (Parent subform)

(* Propositions are capital letters *)
let proposition_subform (p : string) : parseTree =
  let pid = !nPropositions in
  nPropositions := pid + 1;
  let p' =
    { id = pid;
      name = p }
  in
  let subform =
    { valstr = p;
      lchild = Empty ();
      rchild = Empty () }
  in
  propositionList := (!propositionList)@[p'];
  subformulae := (FSet.add (Parent subform) !subformulae);
  (*display_wff (Parent subform);*)
  (Parent subform)

(* ~X *)
let negation_subform (f1 : parseTree) : parseTree =
  let subform = 
    { valstr = "~";
      lchild = Empty ();
      rchild = f1 }
  in
  subformulae := (FSet.add (Parent subform) !subformulae);
  (*display_wff (Parent subform);*)
  (Parent subform)

(* (X*Y) *)
let binary_subform (f1 : parseTree) (f2 : parseTree) (kind : int) : parseTree =
  let str = match kind with
    | 0 -> "=>"
    | 1 -> "<=>"
    | 2 -> "&"
    | 3 -> "|"
    | _ -> assert false
  in
  let subform =
  { valstr = str;
    lchild = f1;
    rchild = f2 }
  in
  subformulae := (FSet.add (Parent subform) !subformulae);
  (*display_wff (Parent subform);*)
  (Parent subform)

(* Invoked after a formula has been completely parsed.
   The formula is stored in a hashtable that maps the formula
   itself to the set containing all of its subformulae. *)
let store_formula (f : parseTree) : unit =
  Hashtbl.add fhtbl f !subformulae;
  subformulae := FSet.empty

(* Prints all the subformulae of a given formula to the terminal. *)
let display_subforms (f : parseTree) : unit =
  let subs = try
    Hashtbl.find fhtbl f
    with Not_found -> (printf "Formula not found: "; display_wff f; assert false;)
  in
  printf "\n\nSubformulae:\n\n";
  FSet.iter display_wff subs