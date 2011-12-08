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

(* The info structure of state of search procedure *)
type info = {
    trans_todo : int list;     (* follow list as numbered choices *)
    choices_so_far : int list; (* numbered choices made so far *)
    trans_so_far : MachineDefTypes.trans list; (* transitions made so far *)
    last_system_state : MachineDefTypes.system_state option; (* the preceding system state (one step up in search tree) *)
    allowed_acts : allowed_reads; (* constraints on read values *)
  }

(* The state interaction manipulates *)
type interaction_state = {
    cands : MachineDefTypes.trans MachineDefTypes.set; (* The set of transitions *)
    info  : info;(* The info structure *)
  }

val display : string -> string -> bool -> unit

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
