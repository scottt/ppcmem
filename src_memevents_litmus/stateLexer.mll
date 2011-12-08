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
open Lexing
open LexMisc
open LexUtils
open StateParser

}

let digit = [ '0'-'9' ]
let alpha = [ 'a'-'z' 'A'-'Z']
let name  = alpha (alpha|digit|'_' | '/' | '.' | '-')*
let num = digit+

rule token = parse
| [' ''\t'] { token lexbuf }
| '\n'      { incr_lineno lexbuf; token lexbuf }
| "(*"      { skip_comment lexbuf ; token lexbuf }
| '-' ? num as num
    {NUM (int_of_string num) }
| 'P' (num as x)
    { PROC (int_of_string x) }
| '%' (name as name) { SYMB_REG name }
| ';' { SEMI }
| ':' { COLON }
| '[' { LBRK }
| ']' { RBRK }
| '(' { LPAR }
| ')' { RPAR }
| '=' { EQUAL }
| '+' { PLUS_DISJ }
| "=>" { IMPLIES }
 | "/\\" {AND}
| "\\/" {OR}
| '~'| "not" { NOT }
| "true"     { TRUE }
| "exists"   { EXISTS }
| "forall"   { FORALL }
| "cases"    { CASES }
| "final"    { FINAL }
| "with"     { WITH }
| "locations" { LOCATIONS }
| "*" { STAR }
| name as name { NAME name }
| eof { EOF }
| "<<" { error "<<" lexbuf }
| "" { error "Init lex" lexbuf }

{
 let token lexbuf =
   let tok = token lexbuf in
   if !Debug.lexer then begin
     Printf.eprintf
       "%a: Lexed '%s'\n"
       Pos.pp_pos2
       (lexeme_start_p lexbuf,lexeme_end_p lexbuf)
       (lexeme lexbuf) ;
     Printf.eprintf
       "LOC=%a\n" Pos.debug_pos (lexeme_start_p lexbuf)
   end ;
   tok
}
