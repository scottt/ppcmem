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

(* Conditions inside logs, a simplified Constraint module *)

open MiscParser
open ConstrGen
open Printf

type constr = MiscParser.constr

let foralltrue =  ForallStates (And [])

let parse s = 
  try
  let lxb = Lexing.from_string s in
  Some (StateParser.constr StateLexer.token lxb)
  with
  | Parsing.Parse_error
  | LexMisc.Error _ -> None

let dump chan = ConstrGen.dump_constraints chan MiscParser.pp_atom


let get_locs c =
  fold_constr (fun (loc,_) k -> LocSet.add loc k)
    c LocSet.empty

(* Code duplication? (with constraints) oh well! *)

module type I = sig
  type v

  type state

  val state_mem : state -> MiscParser.location -> v -> bool
end

module Make(I:I) : sig

  type state = I.state
  type constr = (MiscParser.location, I.v) ConstrGen.prop ConstrGen.constr

  val validate : constr -> state list -> bool
(* Return witness of interest / total number of outcomes *)
  val witness : constr -> (state * Int64.t) list -> Int64.t * Int64.t

end  =
struct

  type state = I.state

  type constr = (MiscParser.location, I.v) ConstrGen.prop ConstrGen.constr


let rec check_prop p state = match p with
| Atom (l,v) -> I.state_mem state l v
| Not p -> not (check_prop p state)
| And ps -> List.for_all (fun p -> check_prop p state) ps
| Or ps -> List.exists (fun p -> check_prop p state) ps
| Implies (p1, p2) -> 
    if check_prop p1 state then check_prop p2 state else true
      
let check_constr c states = match c with
| ForallStates p -> List.for_all (fun s -> check_prop p s) states
| QueryState p -> (* ??? *) assert false
| ExistsState p -> List.exists (fun s -> check_prop p s) states
| NotExistsState p ->
    not (List.exists (fun s -> check_prop p s) states)	      

let validate = check_constr

let witness c states =
  let p = ConstrGen.prop_of c in
  let pos,neg =
    List.fold_left
    (fun (pos,neg) (st,c) ->
      if check_prop p st then
        Int64.add c pos, neg
      else
        pos,Int64.add c neg)
    (Int64.zero,Int64.zero) states in
  match c with
  | ExistsState _
  | NotExistsState _ ->
      pos,neg
  | ForallStates _ ->
      neg,pos
  | QueryState _ -> assert false

end
