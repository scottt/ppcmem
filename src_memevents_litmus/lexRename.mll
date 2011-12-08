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

{
open Printf
open Lexing
open LexMisc
}

let blank = [' ''\t']
let name = [^' ''\t''\n' '%' '#']+
let comment = ('#'|'%') [^'\n']*
rule main parse_value t idx = parse 
| blank* (name as key) blank+ (name as value_pp) blank* comment? '\n'
 {
  let value = match parse_value value_pp with
  | None -> error (sprintf "%s is not a valid value" value_pp) lexbuf
  | Some v -> v in
  if !Misc.verbose > 1 then
    eprintf "LexRename: %s -> (%s,%d)\n" key value_pp idx ;
  let t = TblRename.add_binding t key idx value in
  incr_lineno lexbuf ;
  main parse_value t (idx+1) lexbuf
  }
| (comment | blank*) '\n'
  { incr_lineno lexbuf ; main parse_value t idx lexbuf }
| eof { t }
| "" { error "LexRename" lexbuf }

{

let read fname chan t parse_value =
  let lexbuf = Lexing.from_channel chan in
  lexbuf.lex_curr_p <-
    {pos_fname = fname; pos_lnum = 1;
     pos_bol = 0; pos_cnum = 0};
  try
    main  parse_value t 0 lexbuf
  with LexMisc.Error (msg,loc) ->    
    Printf.eprintf "%a: error in rename map, %s\n"
      Pos.pp_pos loc msg ;
	(*	let warning = Js.Opt.get (Dom_html.window##document##getElementById(Js.string "warnings")) (fun _ -> failwith "FOO") in
			warning##style##display <- Js.string "block";
			warning##innerHTML <-(Js.string*)     
			raise (Misc.Exit (Printf.sprintf "Error Parsing the test (error in rename map,  %s)" msg));
 (* silent, message printed above *)
}
