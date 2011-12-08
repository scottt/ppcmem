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

(* Generic part of constraint [ie the one to be checked
   at the end of test run *)


(* Type of propositions and constraint *)
type ('loc,'v) prop =
    Atom of 'loc * 'v
  | Not of ('loc,'v) prop
  | And of ('loc,'v) prop list
  | Or of ('loc,'v) prop list
  | Implies of ('loc,'v) prop * ('loc,'v) prop

type 'prop constr =
    ForallStates of 'prop
  | ExistsState of 'prop
  | NotExistsState of 'prop
  | QueryState of 'prop

val is_existential : 'prop constr -> bool 
val prop_of : 'prop constr -> 'prop

(* Style of constraints in papers *)
type kind =
  | Allow
  | Forbid
  | Require
  | Unconstrained

val kind_of : 'a constr -> kind
val pp_kind : kind -> string

(* Polymorphic constraint combinators *)

 val fold_constr :
     ('loc * 'v -> 'a -> 'a) -> ('loc,'v) prop constr -> 'a -> 'a

val  map_constr :
    ('loc1 * 'v1 -> 'loc2 * 'v2) ->
      ('loc1,'v1) prop constr ->
          ('loc2,'v2) prop constr

(* Pretty print *)
val result : string ref

type 'atom pp_arg =
    { pp_true : string;
      pp_false : string;
      pp_not : string;
      pp_or : string;
      pp_and : string;
      pp_implies : string;
      pp_mbox : string -> string;
      pp_atom : 'atom -> string; }

val pp_prop : ('loc * 'v) pp_arg -> ('loc,'v) prop  -> string

val dump_constraints : 
    out_channel -> (('loc * 'v) -> string) -> ('loc,'v) prop constr -> unit

val constraints_to_string :
  (('loc * 'v) -> string) -> ('loc,'v) prop constr -> string
