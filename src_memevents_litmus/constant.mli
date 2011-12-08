(*********************************************************************)
(*                          Litmus                                   *)
(*                                                                   *)
(*        Luc Maranget, INRIA Paris-Rocquencourt, France.            *)
(*        Susmit Sarkar, University of Cambridge, UK.                *)
(*                                                                   *)
(*  Copyright 2010 Institut National de Recherche en Informatique et *)
(*  en Automatique and the authors. All rights reserved.             *)
(*  This file is distributed  under the terms of the Lesser GNU      *)
(*  General Public License.                                          *)
(*********************************************************************)

module type S =
  sig
    (* Simpler to have concrete types for litmus *)
    type v =
      | Concrete of int
      | Symbolic of string

    val pp  : v -> string
    val pp_v : v -> string

    val compare : v -> v -> int

    val intToV  : int -> v 
    val nameToV  : string -> v

   val maybevToV : MiscParser.maybev -> v
  end
