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
(*  Sewell, and Pankaj Pawan.                                             *)
(*                                                                        *)
(*  Redistribution and use in source and binary forms, with or without    *)
(*  modification, are permitted provided that the following conditions    *)
(*  are met:                                                              *)
(*  1. Redistributions of source code must retain the above copyright     *)
(*  notice, this list of conditions and the following disclaimer.         *)
(*  2. Redistributions in binary form must reproduce the above copyright  *)
(*  notice, this list of conditions and the following disclaimer in the   *)
(*  documentation and/or other materials provided with the distribution.  *)
(*  3. The names of the authors may not be used to endorse or promote     *)
(*  products derived from this software without specific prior written    *)
(*  permission.                                                           *)
(*                                                                        *)
(*  THIS SOFTWARE IS PROVIDED BY THE AUTHORS ``AS IS'' AND ANY EXPRESS    *)
(*  OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED     *)
(*  WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE    *)
(*  ARE DISCLAIMED. IN NO EVENT SHALL THE AUTHORS BE LIABLE FOR ANY       *)
(*  DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL    *)
(*  DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE     *)
(*  GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS         *)
(*  INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHE   *)
(*  IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR       *)
(*  OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN   *)
(*  IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.                         *)
(*========================================================================*)

open MachineDefUtils
open MachineDefValue
open MachineDefTypes
open Types
open Model_aux
open Printf

type allowed_reads = (address * value) list

(* The info structure of state of search procedure *)
type info = {
    trans_todo : int list;     (* follow list as numbered choices *)
    choices_so_far : int list; (* numbered choices made so far *)
    trans_so_far : trans list; (* transitions made so far *)
    last_system_state : system_state option; (* the preceding system state (one step up in search tree) *)
    allowed_acts : allowed_reads; (* constraints on read values *)
  }

type interactive_input = 
  | I_Choice of info * int
  | I_Auto
  | I_Undo
  | I_Quit

(* The state interaction manipulates *)
type interaction_state = {
    cands : trans set; (* The set of transitions *)
    info  : info;(* The info structure *)
  }


let display x y z =
	()

(* Take interaction data structure as input:
      cands : candidates enabled
      info  : previous info
   Produce interaction data structure (option) as output:
      cands : candidates enabled by user interaction
      info  : updated info
   and feed that to continuation
   OR undo
   OR quit *)
let interact_with_user : Globals.ppmode -> system_state -> interaction_state -> (interaction_state -> unit) -> (unit -> unit) -> (unit -> unit) -> unit =
  fun m s inp k_info k_undo k_userquit ->
          begin
            (match m.Globals.pp_kind with 
            | Globals.Ascii -> Printf.printf "\027[;H\027[J";  (* cursor to top left and clear *)
            | _ -> ());
            Printf.printf "----------------------------------------------------------------------------\n";
            let nmax,numbered_cands = 
              let rec number n xs = match xs with [] -> n,[] | x::xs -> let nmax,ys = number (n+1) xs in nmax, (n,x)::ys in
              number 0 (List.sort compare_trans (Pset.elements inp.cands)) in
(* dead code?
            let itd = 
              { per_instruction = opt_map 
                  (function 
                    | (n,Commit_instruction (tid,ii)) -> Some ((tid,ii.ioid),n)
                    | (n,Read_from_storage_subsystem (tid,ii,w)) -> Some ((tid,ii.ioid),n)
                    | (n,Write_forward_to_read (tid,ii1,w,ii2)) -> Some ((tid,ii1.ioid),n)
                    | _ -> None) 
                  numbered_cands ;
                per_write_ann = opt_map
                  (function 
                    | (n,Write_announce_to_thread (w,tid)) -> Some (w,n)
                    | _ -> None)
                  numbered_cands;
                per_write_coh = opt_map
                  (function 
                    | (n,Write_reaching_coherence_point w) -> Some (w,n)
                    | _ -> None)
                  numbered_cands;
                per_barrier_prop = opt_map 
                  (function 
                    | (n,Barrier_propagate_to_thread (b,tid)) -> Some (b,n)
                    | _ -> None)
                  numbered_cands;
                per_barrier_ack = opt_map
                  (function 
                    | (n,Acknowledge_sync  b) -> Some (b,n)
                    | _ -> None)
                  numbered_cands;
                per_partial_coh_commit = opt_map
                  (function
                    | (n,Partial_coherence_commit (w1,w2)) -> Some ((w1,w2),n)
                    | _ -> None)
                  numbered_cands;
              } in
*)

            let ui_state = Pp.make_ui_system_state_opt (inp.info.last_system_state) s numbered_cands in 
            Printf.printf "%s\n" (Pp.pp_ui_system_state m ui_state);
            (*Printf.printf "%s\n" (Pp.pp_system_state (inp.info.last_system_state) s itd);*)
            Printf.printf "Choices so far: [%s]\n" 
              (String.concat ";" (List.map string_of_int inp.info.choices_so_far));
	    if List.length numbered_cands = 0 then
	      begin
		Printf.printf "No enabled transitions\n";
		k_info {info=inp.info;cands=inp.cands}
	      end
	    else
	      begin
		Printf.printf "Enabled transitions:\n" ;
		List.iter (fun (n,c) -> 
		  
		  Printf.printf "  %2s: %s \n" (Pp.colour_tran_id m (*Pp.pp_tran_id m*) (*({Globals.pp_kind=Globals.Ascii;Globals.pp_colours = (Globals.get_our_runopts()).Globals.colours })*) (sprintf "%2d" n)) (Pp.pp_trans m c)) numbered_cands;
		Printf.printf "----------------------------------------------------------------------------\n";
		
		let rec n_input info = 
		  Printf.printf "Step %d   Choose:  " (List.length inp.info.choices_so_far);
		  (match info.trans_todo with
		  | n::ns -> 
                      Printf.printf "%d: %s (from follow list)\n" n (Pp.pp_trans m (List.assoc n numbered_cands)); 
                      if n <0 || n>= nmax then 
			(Printf.printf "follow specification error: %d must be an integer in [0,%d]\n<shifting to interactive mode>\n" n (nmax-1); 
			 n_input {info with trans_todo = []})
                      else (
			I_Choice ({info with trans_todo = ns},n))
		  | [] -> 
                      let s = try read_line () with End_of_file -> Printf.printf "\n";"quit" in
                      if s = "auto" then 
			I_Auto
                      else if List.mem s ["u";"undo"] then 
			match inp.info.choices_so_far with
			| [] -> Printf.printf "cannot undo from the initial state\n"; n_input info
			| _ -> I_Undo
                      else if List.mem s ["q";"quit"] then 
			I_Quit
                      else
			let n = try int_of_string s with Failure  _ -> -1 in
			if n <0 || n>= nmax then 
			  (Printf.printf "syntax error: must be an integer in [0,%d], \"undo\", \"u\", \"quit\", \"q\", or \"auto\"\n" (nmax-1);
			   n_input info)
			else I_Choice (inp.info,n)) in
		match n_input inp.info with
		| I_Choice(info,n_choice) -> 
                    k_info 
		      {info = {(*inp.*)info with choices_so_far = info.choices_so_far @ [n_choice]};
                       cands = Pset.singleton (trans_compare) (List.assoc  n_choice numbered_cands)}
		| I_Auto -> 
                    (Globals.set_interactive false;
                     k_info inp)
		| I_Undo ->
                    k_undo ()
		| I_Quit ->
                    k_userquit ()
              end 
	  end

let ask_quit_or_undo k_undo k_userquit =
  Printf.printf "----------------------------------------------------------------------------\n";
  let rec inp () = 
    let () = Printf.printf "Undo last transition, to continue exploration, or quit [u/q]: " in
    let s = try read_line () with End_of_file -> Printf.printf "\n"; "quit" in
    if s = "u" || s = "undo" then k_undo ()
    else if s = "q" || s = "quit" then k_userquit ()
    else (Printf.printf "syntax error, please input u/q/undo/quit\n"; inp ()) in
  inp ()

