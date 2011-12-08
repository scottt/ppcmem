(*********************************************************************)
(*                        Memevents                                  *)
(*                                                                   *)
(* Jade Alglave, Luc Maranget, INRIA Paris-Rocquencourt, France.     *)
(* Susmit Sarkar, Peter Sewell, University of Cambridge, UK.         *)
(*                                                                   *)
(*  Copyright 2010 Institut National de Recherche en Informatique et *)
(*  en Automatique and the authors. All rights reserved.             *)
(*  This file is distributed  under the terms of the Lesser GNU      *)
(*  General Public License.                                          *)
(*********************************************************************)


type ('l,'v) prop =
    Atom of 'l * 'v
  | Not of ('l,'v) prop
  | And of ('l,'v) prop list
  | Or of ('l,'v) prop list
  | Implies of ('l,'v) prop * ('l,'v) prop

type 'prop constr =
    ForallStates of 'prop
  | ExistsState of 'prop
  | NotExistsState of 'prop
  | QueryState of 'prop

let is_existential p = match p with
| ExistsState _ -> true
| QueryState _ -> (* ??? *) false
| ForallStates _
| NotExistsState _ -> false

let prop_of = function
| ExistsState p
| QueryState p
| ForallStates p
| NotExistsState p -> p

(* Style of constraints in papers *)
type kind =
  | Allow
  | Forbid
  | Require
  | Unconstrained

let kind_of = function
  | ExistsState _ -> Allow
  | ForallStates _ -> Require
  | NotExistsState _ -> Forbid
  | QueryState _ -> Unconstrained

let pp_kind = function
  | Allow -> "Allowed"
  | Forbid -> "Forbidden"
  | Require -> "Required"
  | Unconstrained -> "Final"

let rec fold_prop f_atom p = match p with
| Atom (x,v) -> f_atom (x,v)
| Not p -> fold_prop f_atom p
| And ps
| Or ps ->
    List.fold_right (fold_prop f_atom) ps
| Implies (p,q) ->
    fun y -> fold_prop f_atom q (fold_prop f_atom p y)
      
let fold_constr f_atom c = match c with
| ForallStates p 
| QueryState p 
| ExistsState p 
| NotExistsState p -> fold_prop f_atom p   

let rec map_prop c_atom p = match p with
| Atom (x,v) ->
    let x, v = c_atom (x,v) in  Atom (x, v)
| Not p ->
    Not (map_prop c_atom p)
| And pl ->
    And (List.map (fun p -> map_prop c_atom p) pl)
| Or pl ->
    Or (List.map (fun p -> map_prop c_atom p) pl)
| Implies (p,q) ->
    Implies (map_prop c_atom p, map_prop c_atom q)

let map_constr f c = match c with
| ForallStates p -> ForallStates (map_prop f p)
| ExistsState p -> ExistsState (map_prop f p)
| NotExistsState p -> NotExistsState (map_prop f p)
| QueryState p -> QueryState (map_prop f p)

(* Pretty print *)

type op = O_top | O_and | O_or | O_implies

let is_paren above here = match above,here with
| O_top,_ -> false
| _,O_top -> assert false
| O_implies,O_implies -> false
| O_implies,(O_or|O_and) -> true
| O_and,O_and -> false
| O_and,(O_or|O_implies) -> true
| O_or,(O_and|O_or) -> false
| O_or,O_implies -> true

let paren above here s =
  if is_paren above here then "(" ^ s ^ ")"
  else s

type 'atom pp_arg =
    { pp_true : string;
      pp_false : string;
      pp_not : string;
      pp_or : string;
      pp_and : string;
      pp_implies : string;
      pp_mbox : string -> string;
      pp_atom : 'atom -> string; }

let rec pp_prop arg =

  let rec pp_prop above p = match p with
  | Atom (l,v) -> arg.pp_atom (l,v)
  | Not p -> arg.pp_not ^ "(" ^ pp_prop O_top p ^ ")"
  | And [] -> arg.pp_true
  | And ps ->
      paren above O_and
        (String.concat (arg.pp_and)  (List.map (pp_prop O_and) ps))
  | Or [] -> arg.pp_false
  | Or ps ->
      paren above O_or
        (String.concat (arg.pp_or)  (List.map (pp_prop O_or) ps))
  | Implies (p1,p2) ->
      paren above O_implies
        (pp_prop O_implies p1 ^
         arg.pp_implies ^
         pp_prop O_implies p2) in
  pp_prop O_top 



let mk_arg pp_atom =
    { pp_true="true";
      pp_false="false";
      pp_not="not " ;
      pp_or=" \\/ " ;
      pp_and=" /\\ " ;
      pp_implies=" => ";
      pp_mbox=(fun s -> s) ;
      pp_atom=pp_atom; }

let dump_prop pp_atom =
  fun chan p -> output_string chan (pp_prop (mk_arg pp_atom) p)

let prop_to_string pp_atom = pp_prop (mk_arg pp_atom)

open Printf
let result = ref ""

let dump_constraints chan pp_atom c = match c with
| ForallStates p -> result := !result ^ Printf.sprintf "forall (%s)" (prop_to_string pp_atom p);
    fprintf chan "forall (%a)" (dump_prop pp_atom) p
| ExistsState p -> result := !result ^ Printf.sprintf  "exists (%s)" (prop_to_string pp_atom p);
    fprintf chan "exists (%a)" (dump_prop pp_atom) p
| NotExistsState p -> result := !result ^ Printf.sprintf "~exists (%s)" (prop_to_string pp_atom p);
    fprintf chan "~exists (%a)" (dump_prop pp_atom) p
| QueryState p -> result := !result ^ Printf.sprintf "(%s)" (prop_to_string pp_atom p);
    fprintf chan "(%a)" (dump_prop pp_atom) p

let constraints_to_string pp_atom c =  match c with
| ForallStates p ->
    sprintf "forall (%s)" (prop_to_string pp_atom p)
| ExistsState p ->
    sprintf "exists (%s)" (prop_to_string pp_atom p)
| NotExistsState p ->
    sprintf "~exists (%s)" (prop_to_string pp_atom p)
| QueryState p ->
    sprintf "(%s)" (prop_to_string pp_atom p)
