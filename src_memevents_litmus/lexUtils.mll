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
 open Index
 open Lexing
 open LexMisc

let debug = 2

let from_section (start_pos,end_pos) ic =
  if !Debug.lexer && debug > 0 then begin
    Printf.eprintf
      "Section: %a -> %a\n" Pos.debug_pos start_pos Pos.debug_pos end_pos ;
  end ;
  (* Start at start_pos *)
  seek_in ic start_pos.pos_cnum ; 

  (* Will hold position of the next refill *)
  let next_pos = ref start_pos.pos_cnum in

  let lexbuf =
    from_function
      (fun buf n -> (* refill function *)
	if !next_pos >= end_pos.pos_cnum then 0 (* pretend eof *)
	else
	  let n_read = input ic buf 0 1 in
	  next_pos := !next_pos + n_read ;
	  (* will trigger refill as soon as end_pos is reached by lexer *)
	  if !next_pos > end_pos.pos_cnum then
	    n_read - (!next_pos - end_pos.pos_cnum)
	  else
	    n_read) in
  if !Debug.lexer && debug > 0 then begin
    Printf.eprintf "start_pos=%a\n" Pos.debug_pos start_pos
  end ;
  (* Initialize position information maintained by lexing engine *)
  lexbuf.lex_curr_p <- start_pos ;
  (* lex_abs_pos is the absolute index of next refill, the lexing
     engine needs this information *)
  lexbuf.lex_abs_pos <- start_pos.pos_cnum ;
  lexbuf
    
let parse_string (start_pos,end_pos) s =
(*	Firebug.console##log(s); *)

	let substring = String.sub s start_pos.pos_cnum (end_pos.pos_cnum - start_pos.pos_cnum) in
		let lexbuf = from_string substring in
(*		Firebug.console##log(Js.string lexbuf.lex_buffer); *)
		lexbuf
}

let digit = ['0'-'9']
let num = digit+
let blank = ['\t'' ']
rule skip_comment i = parse 
  | '\n' { incr_lineno lexbuf; skip_comment i lexbuf }   
  | "(*" { skip_comment (i+1) lexbuf }
  | "*)" 
      { if i > 1 then skip_comment (i-1) lexbuf}
  | eof { error "eof in skip_comment" lexbuf }
  | _ { skip_comment i lexbuf}

and read_index = parse
| "all"
    { Some All }
| '%' (num as x)
    { Some (Modulo (int_of_string x)) }
| num as x
    { Some (Exact (int_of_string x)) }
| (num as x) '-' (num as y)
    { Some (Range (int_of_string x, int_of_string y)) }
| "" { None }


and read_of = parse
| blank+ { read_of lexbuf }
| "of" blank+ { Some true }
| eof { Some false }
| ""  { None }

and read_none = parse
| "none" eof { Some true }
| ""         { Some false }
{

let skip_comment = skip_comment 1

(* Learning monads, now I understand better why
   Haskell forces you to write types. *)
module M : sig
  type 'a t

  (* Undocumented, types are enough. *)
  val (>>) : 'a t -> ('a -> 'b t) -> 'b t
  val choiceT : bool t -> 'a t -> 'a t -> 'a t
  val (!) : 'a -> 'a t

  val read_none : bool t
  val read_of : bool t
  val read_index : index t

  val runT : 'a t -> Lexing.lexbuf -> 'a option

end = struct
  type 'a t = Lexing.lexbuf -> 'a option

  let (>>) f g lexbuf = match f lexbuf with
  | None -> None
  | Some v -> g v lexbuf

  let choiceT mc m1 m2 =
    mc >> fun b -> if b then m1 else m2

  let (!) v _ = Some v

  let runT m lexbuf = m lexbuf

(* Quite heavy in fact *)
  let read_none = read_none
  and read_of = read_of
  and read_index = read_index
end

open M

(* Now I understand why Haskell provides monadic syntax *)
let lex_show_what s =
  let m =
    choiceT read_none
      !Dont
      begin
	read_index >>
	fun idx1 ->
	  choiceT read_of
	    (read_index >> fun idx2 -> !(Do (idx1,idx2)))
	    !(Do (idx1,All))
      end in
  runT m (Lexing.from_string s)

(* Now we know for sure that lexbuf is handled sequentially,
   what a relief! *)
    

}
