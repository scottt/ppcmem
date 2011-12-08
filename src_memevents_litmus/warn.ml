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

open Printf

let prerr_ifverb lvl s =
  if !Misc.verbose > lvl then  begin
    prerr_string "Warning: " ;
    prerr_endline s
  end

let warn fmt = ksprintf  (prerr_ifverb 0) fmt
let warn1 fmt = ksprintf  (prerr_ifverb 1) fmt

let warn_always fmt =
  kfprintf
    (fun chan -> output_char chan '\n' ; flush chan)
    stderr fmt

let prerr_exit s =
  prerr_string "Stop here: " ;
  prerr_endline s ;
  raise (Misc.Exit "Error")

let exit fmt = ksprintf prerr_exit fmt  

let user_error fmt =
  ksprintf (fun msg ->raise  (Misc.UserError msg))  fmt 

let fatal fmt =
  ksprintf (fun msg ->raise  (Misc.Fatal msg))  fmt 
  
