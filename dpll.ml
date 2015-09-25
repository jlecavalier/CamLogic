open Printf
open Set
open Glbdefs

let dpll (clauses : FSetSet.t) : bool =
  let clause = FSetSet.choose clauses in
  let atom = FSet.choose clause in
  false