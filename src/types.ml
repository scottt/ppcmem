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
open MachineDefFreshIds
open MachineDefValue
open MachineDefTypes


(* (\* transitions *\) *)
(* type trans = *)
(*    | Commit_instruction of thread_id * instruction_instance  *)
(*    | Write_announce_to_thread of write * thread_id *)
(*    | Barrier_propagate_to_thread of barrier * thread_id  *)
(*    | Read_from_storage_subsystem of thread_id * instruction_instance * write *)
(*    | Write_forward_to_read of thread_id * instruction_instance * write * instruction_instance *)
(*    | Acknowledge_sync of barrier *)
(*    | Partial_coherence_commit of write * write *)
(*    | Write_reaching_coherence_point of write *)
(*    | Partial_evaluate of thread_id * instruction_instance *)
(*    | Register_read_prev of thread_id * instruction_instance * reg * instruction_instance *)
(*    | Register_read_initial of thread_id * instruction_instance * reg *)
(*    | Choice (\* Not a real transition, for marking choice points in traces *\) *)


type result_type =
    { transitions : trans list;
      state : system_state;
    }



(* ************ UI TYPES ************ *)

(*type ui_trans = int * trans *)

type ppcolour = 
  | UI_removed
  | UI_old
  | UI_new


type ui_trans = int * trans



type ui_storage_subsystem_state =  {
    ui_threads : thread_id list;
    ui_writes_seen_old : write list;
    ui_writes_seen_new : write list;
    ui_coherence_old : (write*write) list;
    ui_coherence_new : (write*write) list;
    ui_writes_past_coherence_point_old : write list ;
    ui_writes_past_coherence_point_new : write list ;
    ui_events_propagated_to : (thread_id * (tracked_event list * tracked_event list)) list;
    ui_unacknowledged_sync_requests_removed : barrier list;
    ui_unacknowledged_sync_requests_old : barrier list; 
    ui_unacknowledged_sync_requests_new : barrier list; 
(*    ui_reservations : (thread_id * write option * ppcolour) list;*)
    ui_ss_transitions : ui_trans list;
    (* invariant: those are always one of:
       | Write_announce_to_thread 
       | Barrier_propagate_to_thread 
       | Acknowledge_sync 
       | Write_reaching_coherence_point 
       | Partial_coherence_commit
    *)
  }

type ui_sem_state = {
    (* ui_remaining_removed : action list; *)
    ui_remaining_now : action list;
    ui_all_changed : bool;
    ui_val_soln_old : solution;
    ui_val_soln_new : solution
  }




type ui_instruction_instance_kind =
  | UI_committed_old
  | UI_committed_new
  | UI_in_flight of ui_trans list  
  (* invariant: those are always one of:
   Commit_instruction, Read_from_storage_subsystem, Write_forward_to_read *)

type writes_read_from =
  | WRF_all_new of write list
  | WRF_some_new of write list * write list 
  | WRF_none

type read_responses =
  | RR_all_new of read_response list
  | RR_some_new of read_response list * read_response list 
  | RR_none

type ui_instruction_instance = {
    ui_ioid : ioid; 
    ui_kind : ui_instruction_instance_kind;
    ui_behaviour : ui_sem_state;  
    ui_regs_in : reg list;      
    ui_regs_out : reg list;     
    ui_read_responses : read_responses;
    ui_writes_read_from : writes_read_from; 
    ui_program_loc : address;     
    ui_instruction : instruction;
    ui_prev_old : (ioid * reaches_by) option ;
    ui_prev_new : (ioid * reaches_by) option ;
  }


type ui_thread_state = {
    ui_thread : thread_id ;                               
    ui_initial_register_state : (reg * value) list ;            
    ui_instructions : ui_instruction_instance list; 
    (*ui_writes_received : write list;                      *)
    ui_unacknowledged_syncs_removed : barrier list ;
    ui_unacknowledged_syncs_old : barrier list ;
    ui_unacknowledged_syncs_new : barrier list
  }

type ui_system_state = {
    ui_model : model_params;
    ui_storage_subsystem : ui_storage_subsystem_state;
    ui_thread_states : ui_thread_state list;
	  (* id_state : id_state;*)
  }

(* *********** END OF UI TYPES ********** *)


