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

(* Pairs of indices , whose interpretation is still dubious *)
type index = All | Exact of int | Range of int * int | Modulo of int
type show_what = Dont | DoUpto of int | Do of index * index

open Printf

let pp_index = function
  | All -> "all"
  | Exact n -> sprintf "%i" n
  | Range (n,m) -> sprintf "%i-%i" n m
  | Modulo n -> sprintf "%%%i" n

let pp_show_what = function
  | Dont -> "none"
  | DoUpto i -> sprintf "upto %i" i
  | Do (i1,i2) ->
    pp_index i1 ^ " of " ^ pp_index i2


