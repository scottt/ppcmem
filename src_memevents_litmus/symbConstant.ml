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

(* This module is common to memevents/litmus *)

(* Warning: in litmus maybevToV must be 'public', as a result
            no symbConstant.mli file can exist. *)


type v =
  | Concrete of int
  | Symbolic of string

let intToV i = Concrete i
and nameToV s = Symbolic s

let maybevToV c = c

let compare c1 c2 = match c1,c2 with
| Concrete i1, Concrete i2 -> Pervasives.compare i1 i2
| Symbolic s1,Symbolic s2 -> String.compare s1 s2
| Concrete _,Symbolic _ -> -1
| Symbolic _,Concrete _ -> 1

let pp = function
  | Concrete i -> string_of_int i
  | Symbolic s -> s

let pp_v = pp
