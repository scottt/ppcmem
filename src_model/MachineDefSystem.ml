(* generated by Lem from MachineDefSystem.lem *)
open Nat_num

type 'a set = 'a Pset.set

(*========================================================================*)
(*                                                                        *)
(*                ppcmem executable model                                 *)
(*                                                                        *)
(*          Susmit Sarkar, University of Cambridge                        *)
(*          Peter Sewell, University of Cambridge                         *)
(*          Jade Alglave, Oxford University                               *)
(*          Luc Maranget, INRIA Rocquencourt                              *)
(*                                                                        *)
(*  This file is copyright 2010,2011 Institut National de Recherche en    *)
(*  Informatique et en Automatique (INRIA), and Susmit Sarkar, Peter      *)
(*  Sewell, and Jade Alglave.                                             *)
(*                                                                        *)
(*  All rights reserved.                                                  *)
(*                                                                        *)
(*                                                                        *)
(*                                                                        *)
(*                                                                        *)
(*                                                                        *)
(*========================================================================*)

(* emacs fontification -*-caml-*- *)

(*: \section{The Whole System Behaviour} :*)

open MachineDefFreshIds
open MachineDefValue
open MachineDefTypes
open MachineDefStorageSubsystem
open MachineDefThreadSubsystem

(* the initial writes should be a list of one write for each address that the program will use *)
let initial_system_state (tids : thread_id set) (irvs: thread_id -> reg -> value) (iws : write list) (m:model_params) =
       { thread_states = (fun tid -> initial_thread_state tid (irvs tid) iws);
          storage_subsystem = initial_storage_subsystem_state m.ss tids iws;
          id_state = initial_id_state;
          model = m;
       }

(*indreln 

forall m s s' l reqs resps.
       (ssmachine_trans m s reqs resps l s')
       ==>
       ssmachine_multitrans m s reqs resps s'

and

forall m s s' s'' l reqs reqs' resps resps'.
       (ssmachine_trans m s reqs resps l s' &&
        ssmachine_multitrans m s' reqs' resps' s'')
       ==>
       ssmachine_multitrans m s (reqs union reqs') (resps union resps') s''*)

(*indreln
(*: One of the threads makes a Thread transition (adding requests and taking responses), and the Storage Subsystem makes one or more transitions (adding responses and taking requests) :*)
forall program s s' tl tid reqs resps.
       ((ssmachine_multitrans s.model.ss s.storage_subsystem reqs resps s'.storage_subsystem) &&
       (thread_trans s.model.t program (s.thread_states tid,s.id_state) reqs resps tl (s'.thread_states tid,s'.id_state)))
       ==>
       system_trans s program s'*)

(*indreln
(*: One of the threads makes a Thread transition (adding requests and taking responses), and the Storage Subsystem makes one or more transitions (adding responses and taking requests) :*)
forall program s s' tl tid reqs resps.
       ((ssmachine_multitrans s.model.ss s.storage_subsystem reqs resps s'.storage_subsystem) &&
       (thread_trans s.model.t program (s.thread_states tid,s.id_state) reqs resps tl (s'.thread_states tid,s'.id_state)))
       ==>
       system_trans_label program s (reqs union resps) s'*)

(*indreln

forall program s s'.
       (system_trans s program s')
       ==>
       system_multitrans s program s'

and

forall program s s' s''.
       (system_trans s program s' &&
        system_multitrans s' program s'')
       ==>
       system_multitrans s program s''*)

(*indreln

forall program s s' tid i.
       (thread_trans s.model.t program (s.thread_states tid,s.id_state) {} {} (T_Fetch i) (s'.thread_states tid,s'.id_state)) &&
       (s.storage_subsystem = s'.storage_subsystem)
       ==>
       system_label_trans program s (Fetch tid i) s'

and 

forall program s s' tid i w.
       (thread_trans s.model.t program (s.thread_states tid,s.id_state) {Wreq w} {} (T_Commit_write i {w}) (s'.thread_states tid,s'.id_state)) &&
       (ssmachine_trans s.model.ss s.storage_subsystem {Wreq w} {} (SS_Accept_write_request w) s'.storage_subsystem)
       ==>
       system_label_trans program s (Commit_write tid i {w}) s'

and

forall program s s' tid i b.
       (thread_trans s.model.t program (s.thread_states tid,s.id_state) {Breq b} {} (T_Commit_barrier i {b}) (s'.thread_states tid,s'.id_state)) &&
       (ssmachine_trans s.model.ss s.storage_subsystem {Breq b} {} (SS_Accept_barrier_request b) s'.storage_subsystem)
       ==>
       system_label_trans program s (Commit_barrier tid i b) s'

and

forall program s s' tid i rws.
       (thread_trans s.model.t program (s.thread_states tid,s.id_state) {} {} (T_Commit_read i rws) (s'.thread_states tid,s'.id_state)) &&
       (s.storage_subsystem = s'.storage_subsystem)
       ==>
       system_label_trans program s (Commit_read tid i rws) s'

and

forall program s s' tid i.
       (thread_trans s.model.t program (s.thread_states tid,s.id_state) {} {} (T_Commit_reg_or_branch i) (s'.thread_states tid,s'.id_state)) &&
       (s.storage_subsystem = s'.storage_subsystem)
       ==>
       system_label_trans program s (Commit_reg_or_branch tid i) s'

and

forall program s s' tid r rr w i.
       (ssmachine_trans s.model.ss s.storage_subsystem {Rreq r} {Rresp rr} (SS_Send_read_response tid r w) s'.storage_subsystem) &&
       (thread_trans s.model.t program (s.thread_states tid,s.id_state) {Rreq r} {Rresp rr} (T_Satisfy_read_reserve i r w) (s'.thread_states tid,s'.id_state))
       ==>
       system_label_trans program s (Read_from_storage_subsystem tid i w) s'

and

forall program s s' tid r rr w i.
       (ssmachine_trans s.model.ss s.storage_subsystem {Rreq r} {Rresp rr} (SS_Send_read_response tid r w) s'.storage_subsystem) &&
       (thread_trans s.model.t program (s.thread_states tid,s.id_state) {Rreq r} {Rresp rr} (T_Satisfy_read_from_storage i r w) (s'.thread_states tid,s'.id_state))
       ==>
       system_label_trans program s (Read_from_storage_subsystem tid i w) s'

and

forall program s s' tid i w iprev.
       (thread_trans s.model.t program (s.thread_states tid,s.id_state) {} {} (T_Satisfy_read_from_forwarded_write i w iprev) (s'.thread_states tid,s'.id_state)) &&
       (s.storage_subsystem = s'.storage_subsystem)
       ==>
       system_label_trans program s (Write_forward_to_read tid i w iprev) s'

and 

forall program s s' w tid.
       (ssmachine_trans s.model.ss s.storage_subsystem {} {} (SS_Propagate_write_to_thread w tid) s'.storage_subsystem) &&
       (s.thread_states = s'.thread_states) &&
       (s.id_state = s'.id_state)
       ==>
       system_label_trans program s (Write_propagate_to_thread w tid) s'

and

forall program s s' b tid.
       (ssmachine_trans s.model.ss s.storage_subsystem {} {} (SS_Propagate_barrier_to_thread b tid) s'.storage_subsystem) &&
       (s.thread_states = s'.thread_states) &&
       (s.id_state = s'.id_state)
       ==>
       system_label_trans program s (Barrier_propagate_to_thread b tid) s'

and

forall program s s' tid b br.
       (ssmachine_trans s.model.ss s.storage_subsystem {} {Back br} (SS_Acknowledge_sync_barrier b) s'.storage_subsystem) &&
       (Back br = barrier_ack_of b) &&
       (tid = b.b_thread) &&
       (thread_trans s.model.t program (s.thread_states tid,s.id_state) {} {Back br} (T_Accept_sync_ack br) (s'.thread_states tid,s'.id_state))
       ==>
       system_label_trans program s (Acknowledge_sync b) s'

and 
  
forall program s s' w1 w2.
       (ssmachine_trans s.model.ss s.storage_subsystem {} {} (SS_Partial_coherence_commit w1 w2) s'.storage_subsystem) &&
       (s.thread_states = s'.thread_states) &&
       (s.id_state = s'.id_state)
       ==>
       system_label_trans program s (Partial_coherence_commit w1 w2) s'

and

forall program s s' w.
       (ssmachine_trans s.model.ss s.storage_subsystem {} {} (SS_Write_reaches_coherence_point w) s'.storage_subsystem) &&
       (s.thread_states = s'.thread_states) &&
       (s.id_state = s'.id_state)
       ==>
       system_label_trans program s (Write_reaching_coherence_point w) s'

and

forall program s s' tid i r v iprev.
       (thread_trans s.model.t program (s.thread_states tid,s.id_state) {} {} (T_Register_read_from_previous_write i r v iprev) (s'.thread_states tid,s'.id_state)) &&
       (s.storage_subsystem = s'.storage_subsystem)
       ==>
       system_label_trans program s (Register_read_prev tid i r iprev) s'

and 

forall program s s' tid i r v.
       (thread_trans s.model.t program (s.thread_states tid,s.id_state) {} {} (T_Register_read_from_initial i r v) (s'.thread_states tid,s'.id_state)) &&
       (s.storage_subsystem = s'.storage_subsystem)
       ==>
       system_label_trans program s (Register_read_initial tid i r) s'

and 

forall program s s' tid i.
       (thread_trans s.model.t program (s.thread_states tid,s.id_state) {} {} (T_Partial_evaluate i) (s'.thread_states tid,s'.id_state)) &&
       (s.storage_subsystem = s'.storage_subsystem)
       ==>
       system_label_trans program s (Partial_evaluate tid i) s'

and 

forall program s s' tid i.
       (thread_trans s.model.t program (s.thread_states tid,s.id_state) {} {} (T_Restart i) (s'.thread_states tid,s'.id_state)) &&
       (s.storage_subsystem = s'.storage_subsystem)
       ==>
       system_label_trans program s (Restart_instruction_instance tid i) s'

and 

forall program s s' tid i.
       (thread_trans s.model.t program (s.thread_states tid,s.id_state) {} {} (T_Abort i) (s'.thread_states tid,s'.id_state)) &&
       (s.storage_subsystem = s'.storage_subsystem)
       ==>
       system_label_trans program s (Abort_instruction_instance tid i) s'*)
