open Printf
open Set
open Glbdefs

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

let to_str (pair : (parseTree * bool)) : unit =
  let (pt, b) = pair in
  if b then (printf "%s |--> TRUE\n" (wff_to_string pt)) else (printf "%s |--> FALSE\n" (wff_to_string pt))

let isolate_vars (pair : (parseTree * bool)) : bool =
  let (pt, _) = pair in
  let str = wff_to_string pt in
  (String.length str) = 1

let display_interpretation (interpretation : (parseTree * bool) list) (sat : bool) (mode : bool) : unit =
  if sat then begin
  	let clean_interp = List.sort_uniq Pervasives.compare interpretation in
  	let final = List.filter isolate_vars clean_interp in
    let note = if mode then "\nSatisfying interpretation:\n" 
    else "\nCounterexample:\n" in
    printf "%s" note;
    List.iter to_str final
  end