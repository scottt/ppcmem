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

(**********************************)
(* dumping of view orders control *)
(**********************************)

(* index in a sequence *)
type index =
  | All (* Everything *)
  | Exact of int (* This item *)
  | Range of int * int (* Items [n1..n2] *)
  | Modulo of int      (* Every n item *)

(* control dumping of view orders *)
type show_what =
| Dont (* dump nothing *)
| DoUpto of int   (* Dump up to so many *)
| Do of index * index (* first index is for vo, second for event structure *)

val pp_index : index -> string
val pp_show_what : show_what -> string

