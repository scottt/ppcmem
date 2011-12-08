(*========================================================================*)
(*                                                                        *)
(*             ppcmem model exploration tool                              *)
(*                                                                        *)
(*          Susmit Sarkar, University of Cambridge                        *)
(*          Peter Sewell, University of Cambridge                         *)
(*          Luc Maranget, INRIA Rocquencourt                              *)
(*          Pankaj Pawan, INRIA Rocquencourt                              *)
(*          Francesco Zappa Nardelli, INRIA Rocquencourt                  *)
(*                                                                        *)
(*  This file is copyright 2010,2011 Institut National de Recherche en    *)
(*  Informatique et en Automatique (INRIA), Susmit Sarkar, Peter          *)
(*  Sewell, and Pankaj Pawan                                              *)
(*                                                                        *)
(*  All rights reserved.                                                  *)
(*                                                                        *)
(*                                                                        *)
(*                                                                        *)
(*                                                                        *)
(*                                                                        *)
(*========================================================================*)

open MachineDefTypes

val debug : int ref
val follow : int list ref

type ppkind =
  | Ascii
  | Html
  | Latex

type pparch =
  | PP_PPC
  | PP_ARM

type runopts =
    { 
     model_params:MachineDefTypes.model_params;
     tex_macros:bool;
     interactive:bool;
     colours:bool;
     ppkind:ppkind;
     pparch:pparch;
     web_diagrams:bool;
     safemode:bool;
     statematchmode:bool;
     fast:bool;
    }

val get_our_runopts : unit -> runopts

val set_model : MachineDefTypes.model_params -> unit
val set_interactive : bool -> unit
val set_fast : bool -> unit
val set_colours : bool -> unit
val set_pparch : pparch -> unit
val set_safemode : bool -> unit
val set_statematchmode : bool -> unit



type ppmode = 
  { pp_kind : ppkind;
    pp_colours : bool;
    pp_arch : pparch}

val get_our_ppmode : unit -> ppmode

val init_write_event_ids : MachineDefTypes.w_eiid list ref

(* 32/64 mode (impacts on code generation) *)
type word = W32 | W64 | WXX

val get_word :  Misc.arch -> word
val set_word : string -> unit
val default_ws : unit -> string

(* Run name - identifying string *)
val get_our_runname : unit -> string
val set_our_runname : string -> unit

val set_ppkind : string -> unit

val set_pparch : pparch -> unit

(* Sync macro replacement *)
val sync_const : int (* One place for magic constant 128 *)

val pp_ppkind : ppkind -> string

exception Interactive_quit

(* Dont output *)

val auto : bool ref
