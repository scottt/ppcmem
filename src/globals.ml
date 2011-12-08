(*========================================================================*)
(*                                                                        *)
(*             ppcmem model exploration tool                              *)
(*                                                                        *)
(*          Susmit Sarkar, University of Cambridge                        *)
(*          Peter Sewell, University of Cambridge                         *)
(*          Luc Maranget, INRIA Rocquencourt                              *)
(*                                                                        *)
(*  This file is copyright 2010,2011 Institut National de Recherche en    *)
(*  Informatique et en Automatique (INRIA), Susmit Sarkar, and Peter      *)
(*  Sewell.                                                               *)
(*                                                                        *)
(*  All rights reserved.                                                  *)
(*                                                                        *)
(*                                                                        *)
(*                                                                        *)
(*                                                                        *)
(*                                                                        *)
(*========================================================================*)

open MachineDefTypes

let debug = ref 0


let follow = ref ([] : int list)

type ppkind =
  | Ascii
  | Html
  | Latex

let ppkinds = [Ascii;Html;Latex]

let pp_ppkind k = match k with
| Ascii -> "Ascii"
| Html -> "Html"
| Latex -> "Latex"

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
      
let default_runopts =
    {
     model_params =
     { ss = { coherence_commit = Partial_CC;
	      pcc_deadlock_avoid = true; 
              coherence_points = true};
       t = { thread_loose_tight = Thread_loose;
             thread_restart_forwarded = Restart_forwarded_reads;
             thread_lwsync = Lwsync_read_block};
     };
     tex_macros = false;
     interactive = false;
     colours = false;
     ppkind= Ascii;
     pparch= PP_PPC;
     web_diagrams = false;
     safemode = true;
     statematchmode = false;
     fast = false ;
 }

let our_runopts = ref default_runopts

let get_our_runopts () = !our_runopts

let set_model m = our_runopts := { !our_runopts with model_params = m}

let set_interactive b = our_runopts := { !our_runopts with interactive = b}

let set_fast b = our_runopts := { !our_runopts with fast = b}

let set_colours b = our_runopts := { !our_runopts with colours = b}

let set_ppkind k = our_runopts := { 
  !our_runopts with ppkind = 
                                    match k with 
                                    | "Ascii" -> Ascii
                                    | "Latex" -> Latex
                                    | "Html" -> Html
                                    | _ -> raise (Failure ("ppkind must be one of Ascii, Latex, or Html"))
                                  }

let set_pparch a = our_runopts := { 
  !our_runopts with pparch = a }

let set_safemode b = our_runopts := { !our_runopts with safemode = b}
let set_statematchmode b = our_runopts := { !our_runopts with statematchmode = b}

type ppmode = 
  { pp_kind : ppkind;
    pp_colours : bool;
    pp_arch : pparch}

let get_our_ppmode () = 
  {pp_kind=(get_our_runopts()).ppkind;
   pp_colours = (get_our_runopts()).colours;
   pp_arch = (get_our_runopts()).pparch } 

(* for pretty-printing only *)
let init_write_event_ids = ref ([]:MachineDefTypes.w_eiid list)
(*let init_write_events = ref ([]:MachineDefTypes.w_eiid list)*)


type word = W32 | W64 |WXX

let set_word tag = () (* Do nothing now *)

let get_word arch = W32 (* Hardcoded *)

let do_pp_ws = function
  | W32 -> "w32"
  | W64 -> "w64"
  | WXX -> "-"

let default_ws () = do_pp_ws W32

(************)
(* run name *)
(************)

let runname = ref "default"
let set_our_runname s = runname := s
let get_our_runname () = !runname


(***********************)
(* PPC Sync Macro      *)
(***********************)

let sync_const = 128

exception Interactive_quit

let auto = ref false
