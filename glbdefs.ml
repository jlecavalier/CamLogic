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

module FSetSet = Set.Make(
  struct
    let compare =
  Pervasives.compare
    type t = FSet.t
  end)