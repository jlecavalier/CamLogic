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

let () =
(*
  printf "\n\n----TESTING BCP----\n\n";

  clauses := FSetSet.add (FSet.singleton f1) !clauses;

  clauses := FSetSet.add (FSet.singleton f6) !clauses;

  clause := FSet.add f2 !clause;
  clause := FSet.add f3 !clause;
  clause := FSet.add f5 !clause;
  clauses := FSetSet.add !clause !clauses;
  clause := FSet.empty;

  clause := FSet.add f2 !clause;
  clause := FSet.add f3 !clause;
  clause := FSet.add f6 !clause;
  clauses := FSetSet.add !clause !clauses;
  clause := FSet.empty;

  printf "\nBEFORE BCP:\n";
  display_cnf_clauses ();
  clauses := bcp !clauses;
  printf "\nAFTER BCP:\n";
  display_cnf_clauses ();

  clauses := FSetSet.empty;
  clause := FSet.empty;

  printf "\n\n----TESTING CHOOSE_VAR----\n\n";

  clause := FSet.add f2 !clause;
  clause := FSet.add f4 !clause;
  clause := FSet.add f6 !clause;
  clauses := FSetSet.add !clause !clauses;
  clause := FSet.empty;

  printf "\nCLAUSES:\n";
  display_cnf_clauses ();
  printf "\nCHOOSING VAR:\n";
  display_wff (choose_var !clauses);

  printf "\n\n----TESTING SUBSTITUTE----\n\n";

  printf "\nBEFORE SUBSTITUTION\n";
  display_cnf_clauses ();
  clauses := substitute !clauses f3 false;
  printf "\nAFTER SUBSTITUTION\n";
  display_cnf_clauses ();
  *)
  printf "\n\n----TESTING DPLL----\n\n";

  clauses := FSetSet.add (FSet.singleton f1) !clauses;

  clauses := FSetSet.add (FSet.singleton f6) !clauses;

  clause := FSet.add f2 !clause;
  clause := FSet.add f3 !clause;
  clause := FSet.add f5 !clause;
  clauses := FSetSet.add !clause !clauses;
  clause := FSet.empty;

  clause := FSet.add f2 !clause;
  clause := FSet.add f3 !clause;
  clause := FSet.add f6 !clause;
  clauses := FSetSet.add !clause !clauses;
  clause := FSet.empty;

  printf "\nBEFORE DPLL:\n";
  display_cnf_clauses ();

  (*let sat = (Dpll.dpll !clauses) in

  ()

  let str = if sat then "\nSATISFIABLE\n" else "\nUNSATISFIABLE\n" in

  print_endline str;
  
  printf "\nAFTER DPLL:\n";
  
  display_cnf_clauses ();

  clauses := FSetSet.empty;
  clause := FSet.empty;*)