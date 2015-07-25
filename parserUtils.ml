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
let clauses : FSet.t list ref = ref [] (* The list of clauses to resolve *)
let nPropositions : int ref = ref 0 (* number of propositions we're working with *)
let linenum : int ref  = ref 1 (* The line number we're parsing. *)
let subformulae = ref FSet.empty (* A set of subformulae *)
let clause = ref FSet.empty (* The current clause *)
let fhtbl = Hashtbl.create 5 (* Maps formulae to their set of subformulae *)

(* Notify the user of errors. *)
let warn str = fprintf stderr "@L%d: Warning: %s \n" !linenum str
let fatal str = fprintf stderr "@L%d: Fatal: %s \n" !linenum str; assert false

(* Prints a formula to the terminal screen *)
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

let rep (f : parseTree) : parseTree =
  match f with
  | Empty () -> assert false
  | Parent node -> begin
  	match node.rchild with
  	| Empty () -> f
  	| _ -> begin
  	  let str = (sprintf "rep(%s)" (wff_to_string f)) in
  	  let pid = !nPropositions in
      nPropositions := pid + 1;
  	  let p' = {id = pid; name = str} in
  	  propositionList := (!propositionList)@[p'];
  	  Parent { valstr = str; lchild = Empty (); rchild = Empty () }
  	end
  end

let display_all_reps (f : parseTree) : unit =
  printf "Representatives:\n";
  let subs = try
    Hashtbl.find fhtbl f
    with Not_found -> (printf "Formula not found: "; display_wff f; assert false;)
  in
  FSet.iter (fun x -> display_wff (rep x)) subs

let update_clauses () : unit =
  clauses := (!clauses)@[!clause];
  clause := FSet.empty

let enc (f : parseTree) : parseTree =
  match f with
  | Empty () -> assert false
  | Parent node -> begin
  	match node.rchild with
  	| Empty () -> Parent {valstr = "TRUE"; lchild = Empty (); rchild = Empty ()}
  	| _ -> begin
  	  match node.valstr with
  	  | "~" -> begin
  	  	let rf = rep f in
  	  	let nrf = Parent {valstr = "~"; lchild = Empty (); rchild = rf} in
  	  	let rcf = rep node.rchild in
  	  	let nrcf = Parent {valstr = "~"; lchild = Empty (); rchild = rcf} in
  	  	clause := FSet.add nrf !clause;
  	  	clause := FSet.add nrcf !clause;
  	  	update_clauses ();
  	  	clause := FSet.add rf !clause;
  	  	clause := FSet.add rcf !clause;
  	  	update_clauses ();
  	  	let c1 = Parent {valstr = "|"; lchild = nrf; rchild = nrcf} in
  	  	let c2 = Parent {valstr = "|"; lchild = rf; rchild = rcf} in
  	  	Parent {valstr = "&"; lchild = c1; rchild = c2}
  	  end
  	  | "&" -> begin
        let p = rep f in
        let np = Parent {valstr = "~"; lchild = Empty (); rchild = p} in
        let rf1 = rep node.lchild in
        let rf2 = rep node.rchild in
        let nrf1 = Parent {valstr = "~"; lchild = Empty (); rchild = rf1} in
        let nrf2 = Parent {valstr = "~"; lchild = Empty (); rchild = rf2} in
        clause := FSet.add np !clause;
        clause := FSet.add rf1 !clause;
        update_clauses ();
        clause := FSet.add np !clause;
        clause := FSet.add rf2 !clause;
        update_clauses ();
        clause := FSet.add nrf1 !clause;
        clause := FSet.add nrf2 !clause;
        clause := FSet.add p !clause;
        update_clauses ();
        let c1 = Parent {valstr = "|"; lchild = np; rchild = rf1} in
        let c2 = Parent {valstr = "|"; lchild = np; rchild = rf2} in
        let d1_c3 = Parent {valstr = "|"; lchild = nrf1; rchild = nrf2} in
        let c3 = Parent {valstr = "|"; lchild = d1_c3; rchild = p} in
        let conj1 = Parent {valstr = "&"; lchild = c1; rchild = c2} in
        Parent {valstr = "&"; lchild = conj1; rchild = c3}
  	  end
  	  | "|" -> begin
  	  	let p = rep f in
  	  	let np = Parent {valstr = "~"; lchild = Empty (); rchild = p} in
  	  	let rf1 = rep node.lchild in
  	  	let rf2 = rep node.rchild in
  	  	let nrf1 = Parent {valstr = "~"; lchild = Empty (); rchild = rf1} in
        let nrf2 = Parent {valstr = "~"; lchild = Empty (); rchild = rf2} in
        clause := FSet.add np !clause;
        clause := FSet.add rf1 !clause;
        clause := FSet.add rf2 !clause;
        update_clauses ();
        let d1_c1 = Parent {valstr = "|"; lchild = np; rchild = rf1} in
        let c1 = Parent {valstr = "|"; lchild = d1_c1; rchild = rf2} in
        clause := FSet.add nrf1 !clause;
        clause := FSet.add p !clause;
        update_clauses ();
        let c2 = Parent {valstr = "|"; lchild = nrf1; rchild = p} in
        clause := FSet.add nrf2 !clause;
        clause := FSet.add p !clause;
        update_clauses ();
        let c3 = Parent {valstr = "|"; lchild = nrf2; rchild = p} in
        let conj1 = Parent {valstr = "&"; lchild = c1; rchild = c2} in
        Parent {valstr = "&"; lchild = conj1; rchild = c3}
  	  end
  	  | _ -> assert false
  	end
  end

let display_enc (f : parseTree) : unit =
  printf "Encodings:\n";
  display_wff (enc f)