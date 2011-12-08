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

(* Constraints on reading values *)

type allowed_reads = (MachineDefTypes.address * MachineDefValue.value) list

val system_state : Dom_html.divElement Js.t 
val litmus_test : Dom_html.textAreaElement Js.t
val transitions : Dom_html.divElement Js.t
val warnings : Dom_html.divElement Js.t
val options : Dom_html.divElement Js.t

val next : Dom_html.inputElement Js.t
val interactive : Dom_html.inputElement Js.t
val non_interactive : Dom_html.inputElement Js.t
val test_select : Dom_html.inputElement Js.t
val undo : Dom_html.inputElement Js.t
val auto : Dom_html.inputElement Js.t
val reset : Dom_html.inputElement Js.t
val help : Dom_html.inputElement Js.t
val select_options : Dom_html.inputElement Js.t
val replace_text : Js.js_string Js.t -> string -> string -> Js.js_string Js.t

val display : string -> string -> bool -> unit

(* The info structure of state of search procedure *)
type info = {
    mutable trans_todo : int list;     (* follow list as numbered choices *)
    mutable choices_so_far : int list; (* numbered choices made so far *)
    mutable trans_so_far : MachineDefTypes.trans list; (* transitions made so far *)
    mutable last_system_state : MachineDefTypes.system_state option; (* the preceding system state (one step up in search tree) *)
    mutable allowed_acts : allowed_reads; (* constraints on read values *)
  }

(* The state interaction manipulates *)
type interaction_state = {
    mutable cands :  MachineDefTypes.trans MachineDefTypes.set; (* The set of transitions *)
    mutable info  : info;(* The info structure *)
  }

(* main interaction function 
 * Arguments:
 * m -- pretty printing mode
 * s -- system state that is being processed
 * info -- interaction state on input
 * normal_cont -- continuation in the normal case, a function 
                  taking the new interaction state as input
 * undo_cont -- function to call if user wants to undo
 * quit_cont -- function to call if user wants to quit
 *)
val interact_with_user :
  Globals.ppmode ->
  MachineDefTypes.system_state ->
  interaction_state ->
  (interaction_state -> unit) ->
  (unit -> unit) ->
  (unit -> unit) -> unit

(* function called at end of interactive search,
   asking whether to quit or undo
 * Arguments:
 * undo_cont -- function to call if user wants to undo
 * quit_cont -- function to call if user wants to quit
 *)
val ask_quit_or_undo :
  (unit -> unit) -> (unit -> unit) -> unit
