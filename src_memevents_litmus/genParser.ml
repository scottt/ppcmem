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

(******************************)
(* A 'generic' parsing module *)
(******************************)


(* input signature, a lexer and a parser for a given architecture *)
module type LexParse = sig
  type token
  type instruction

  val lexer : Lexing.lexbuf -> token
  val parser_ :
       (Lexing.lexbuf -> token) -> Lexing.lexbuf ->
	 int list * instruction list list
	 
end

(* Output signature *)
module type S = sig
  type pseudo
  type init = MiscParser.state
  type prog = (int * pseudo list) list
  type locations = MiscParser.LocSet.t

  val parse_init : Lexing.lexbuf -> init
  val parse_prog : Lexing.lexbuf -> prog
  val digest : init -> prog -> locations -> string


  val parse_web : string -> Splitter.result ->  pseudo MiscParser.t
  val parse : in_channel -> Splitter.result ->  pseudo MiscParser.t
end


module Make
    (A:ArchBase.S)
    (L: LexParse
    with type instruction = A.pseudo) : S with type pseudo = A.pseudo =
  struct
    type pseudo = A.pseudo
    type init = MiscParser.state
    type prog = (int * pseudo list) list
    type locations = MiscParser.LocSet.t

    open Lexing
    open Printf

(*
  Transpose the instructions:
  a list of rows -> a list of columns (each being the program
  for a given processor
*)
    let transpose procs prog =
      try
	let prog = Misc.transpose prog in
	List.combine procs prog 
      with
      |  Misc.TransposeFailure | Invalid_argument "List.combine" ->
	  Warn.fatal "mismatch in instruction lines"


(************************)
(* Various basic checks *)
(************************)

let check_procs procs =
  Misc.iteri
    (fun k p ->
      if k <> p then
        Warn.fatal "Processes must be P0, P1, ...")
    procs

let check_loc procs loc = match loc with
| MiscParser.Location_reg (p,_) ->
    if not (List.mem p procs) then
      Warn.fatal "Bad process P%i" p
| _ -> ()

let check_regs procs init locs final =
  List.iter (fun (loc,_) -> check_loc procs  loc) init ;
  List.iter (fun (loc,_) -> check_loc procs  loc) locs ;
  ConstrGen.fold_constr
    (fun (loc,_) () -> check_loc procs loc)
    final ()

(*******************)
(* Macro expansion *)  
(*******************)

    let rec expn  = function
      | [] -> []
      | A.Macro (name,regs)::k ->
          let f =
            try A.get_macro name
            with Not_found -> Warn.fatal "macro not found: %s" name in
          f regs (expn k)
      | i::k -> i::expn k

    let expn_prog =
      Label.reset () ;
      List.map (fun (p,code) -> p,expn code)


(***********)
(* Parsing *)
(***********)


    let call_parser_std name lexbuf lex parse =
      try parse lex lexbuf
      with
      | LexMisc.Error (msg,pos) ->
	  Printf.eprintf
	    "%a: Lex error %s (in %s)\n" Pos.pp_pos pos msg name ;
	  raise (Misc.Exit (Printf.sprintf "Error Parsing the Test (Lex error %s (in %s))" msg name)) 
      | Parsing.Parse_error ->
	  let lxm = lexeme lexbuf
	  and start_loc = lexeme_start_p lexbuf
	  and end_loc = lexeme_end_p lexbuf in
	  Printf.eprintf
	    "%a: unexpected '%s' (in %s)\n"
	    Pos.pp_pos2 (start_loc,end_loc)
	    lxm name ;
	  raise (Misc.Exit (Printf.sprintf "Error Parsing the Test (unexpected '%s' (in %s))" lxm name))
      | e ->
	  Printf.eprintf
	    "%a: Uncaught exception %s (in %s)\n"
	    Pos.pp_pos lexbuf.lex_curr_p
	    (Printexc.to_string e) name ;
	  assert false

    let parse_init lexbuf =
      call_parser_std "init" lexbuf StateLexer.token StateParser.init

    let parse_prog lexbuf =
      let procs,prog = call_parser_std "prog" lexbuf L.lexer L.parser_ in
      check_procs procs ;
      let prog = transpose procs prog in
      let prog = expn_prog prog in
      prog

    let call_parser name chan loc =
      let lexbuf = LexUtils.from_section loc chan in 
      call_parser_std name lexbuf
    
	
	let call_parser_web name chan loc =
      let lexbuf = LexUtils.parse_string loc chan in
      call_parser_std name lexbuf


    module D = TestHash.Make(A)
    let digest = D.digest

	let parse_web chan 
        {
         Splitter.locs = (init_loc, prog_loc,constr_loc,_) ;
         info = info ;
       }  =
      let init =
		call_parser_web "init"
	  	chan init_loc StateLexer.token StateParser.init in
      let procs,prog =
		call_parser_web "prog" chan prog_loc L.lexer L.parser_ in
      	check_procs procs ;

     	let prog = transpose procs prog in
      	let prog = expn_prog prog in
      	let (locs,final,quantifiers) =
		call_parser_web "final"
		  chan constr_loc StateLexer.token StateParser.constraints in
    	  check_regs procs init locs final ;
      	let all_locs =
        MiscParser.LocSet.union
          ( MiscParser.LocSet.of_list (List.map fst locs))
          (LogConstr.get_locs final) in
      	{
      	 MiscParser.info = ("Hash",digest init prog all_locs)::info ;
      	 init = init ;
      	 prog = prog;
       	 condition = final; 
      	 kinds = quantifiers;
      	 locations = locs;
     	}

    let parse chan 
        {
         Splitter.locs = (init_loc, prog_loc,constr_loc,_) ;
         info = info ;
       }  =
      let init =
	call_parser "init"
	  chan init_loc StateLexer.token StateParser.init in
      let procs,prog =
	call_parser "prog" chan prog_loc L.lexer L.parser_ in
      check_procs procs ;

      let prog = transpose procs prog in
      let prog = expn_prog prog in
      let (locs,final,quantifiers) =
	call_parser "final"
	  chan constr_loc StateLexer.token StateParser.constraints in
      check_regs procs init locs final ;
      let all_locs =
        MiscParser.LocSet.union
          ( MiscParser.LocSet.of_list (List.map fst locs))
          (LogConstr.get_locs final) in
      {
       MiscParser.info = ("Hash",digest init prog all_locs)::info ;
       init = init ;
       prog = prog;
       condition = final; 
       kinds = quantifiers;
       locations = locs;
     }
  end
          
