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

(*********************************************************)
(* A 'generic' parsing module for memevents/litmus files *)
(*********************************************************)

(* input signature, a lexer and a parser for a given architecture *)
module type LexParse = sig
  type token
  type instruction

  val lexer : Lexing.lexbuf -> token
  val parser_ :
       (Lexing.lexbuf -> token) -> Lexing.lexbuf ->
	 int list * instruction list list	 
end

module type S = sig
  type pseudo
  type init = MiscParser.state
  type prog = (int * pseudo list) list
  type locations = MiscParser.LocSet.t

(* Partial access for external digest computation *)
  val parse_init : Lexing.lexbuf -> init
  val parse_prog : Lexing.lexbuf -> prog
  val digest : init -> prog -> locations -> string

(* Main parser for memevents and litmus *)
  val parse_web : string -> Splitter.result ->  pseudo MiscParser.t
  val parse : in_channel -> Splitter.result ->  pseudo MiscParser.t
end

(* Build a generic parser *)
module Make
    (A:ArchBase.S)
    (L: LexParse with type instruction = A.pseudo) :
    S with type pseudo = A.pseudo
