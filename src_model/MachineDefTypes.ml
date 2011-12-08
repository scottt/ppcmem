(* generated by Lem from MachineDefTypes.lem *)
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

(* naming convention: load and store are used for instructions; read
and write for memory actions.  Unqualifed load/store/read/write do not
include any reserve/conditional or acquire/release variants *)


open MachineDefUtils
open MachineDefFreshIds
open MachineDefValue

(*: \section{The Core/Storage Subsystem Interface} :*) 

type thread_id = num
type barrier_type = Sync | LwSync | Eieio
  
type address = value

(* execution instance ids, w_eiid, r_eiid, and b_eiid, are treated
abstractly in the model (they are compared with equality but their
fields are not accessed) but constructed deterministically rather
than being gensym'd *)

type w_eiid = { weiid_thread:thread_id; weiid_ioid:ioid; weiid_addr:address;weiid_value:value }
type r_eiid = { reiid_thread:thread_id; reiid_ioid:ioid; reiid_addr:address } 
type b_eiid = { beiid_thread:thread_id; beiid_ioid:ioid }

type write = { w_thread:thread_id; w_ioid:ioid; w_eiid:w_eiid ; w_addr:address; w_value:value ; w_isrelease:bool }
type write_conditional_request = { wc_w:write; wc_wprev:write option}

type barrier = { b_thread:thread_id; b_ioid:ioid; b_eiid:b_eiid; b_barrier_type:barrier_type } 
type barrier_ack = { br_thread:thread_id; br_ioid:ioid; br_eiid:b_eiid }

type read_request =  { r_thread:thread_id; r_ioid:ioid; r_eiid:r_eiid ; r_addr:address }
type read_response = { rr_thread:thread_id; rr_ioid:ioid; rr_eiid:r_eiid; rr_write:write }

type message =
        (* write request from thread*)
       | Wreq of write

        (* write-conditional request from thread *)
       | WCreq of write_conditional_request

        (* write-conditional response to thread *)
       | WCresp of bool

         (* barrier request from thread*)
       | Breq of barrier

         (* barrier ack sent to thread for sync *)
       | Back of barrier_ack

	 (* read request from thread *)
       | Rreq of read_request

         (* read response to thread *)
       | Rresp of read_response




(*: \section{The Storage Subsystem Model} :*) 

(*: \subsection{Storage Subsystem States} :*) 

type tracked_event =
       | SWrite of write
       | SBarrier of barrier

type storage_subsystem_state = 
     {
  (*: the set of thread ids that exist in the system :*)
     threads : thread_id set;
    
    writes_seen : write set;

  (*: for each address, a strict partial order over the writes to
      that address that the storage subsystem has received from the
      threads, giving the current constraints on the coherence
      order over those writes. We record the union of those
      orders. We encode the
      partial order as a set of pairs of writes. :*)
    coherence : (write*write) set;
    
    (*: the set of writes that have reached their coherence points. :*)
    writes_past_coherence_point : write set ;
    
    (*: for each thread, the writes that have been propagated to it by the
      storage subsystem, together with the barriers that have been
      propagated to that thread. These are all placed in a
      linear (per-thread) order corresponding to that thread's view of
      time, most recent at the end of the list.  :*)
    events_propagated_to : thread_id -> tracked_event list;
      
    (*: the set of [[sync]] barriers that have not yet been acknowledged
        back to their originating thread :*)
    unacknowledged_sync_requests : barrier set;
}
  

(*: \subsection{Instructions and their semantics} :*)

(*: \subsubsection{Assembly instruction abstract syntax} :*)

(*: Registers :*)

type ireg =
  | GPR0 | GPR1 | GPR2 | GPR3
  | GPR4 | GPR5 | GPR6 | GPR7
  | GPR8 | GPR9 | GPR10 | GPR11
  | GPR12 | GPR13 | GPR14 | GPR15
  | GPR16 | GPR17 | GPR18 | GPR19
  | GPR20 | GPR21 | GPR22 | GPR23
  | GPR24 | GPR25 | GPR26 | GPR27
  | GPR28 | GPR29 | GPR30 | GPR31

type ppc_reg = 
  | Ireg of ireg (* integer registers *)
  | PC
  | CRBit of num (* from [0..31] *)

type arm_reg = 
  | R0 | R1 | R2 | R3
  | R4 | R5 | R6 | R7
  | R8 | R9 | R10 | R11
  | R12 
  | SP | LR | ARM_PC
  | Z  (* condition flags *)

type reg = 
  | PPC_reg of ppc_reg
  | ARM_reg of arm_reg

(*: Instructions :*)

type k = num
type lbl = string

(*: PPC Instructions :*)

type idx = num

type cond =
  | Eq | Ne
  | Lt | Ge
  | Gt | Le

type crfindex = num

type setcr0 = SetCR0 | DontSetCR0

(* TODO: some of the following uses of reg should be ireg, e.g. for arithmetic instructions *)
type ppc_instruction =

(* a pseudoinstruction *)
  | Plabel of lbl  

(* These instructions have two forms, setting cr0 or not *)
  | Padd of setcr0*reg*reg*reg
  | Psub of setcr0*reg*reg*reg
  | Por of setcr0*reg*reg*reg
  | Pand of setcr0*reg*reg*reg
  | Pxor of setcr0*reg*reg*reg
  | Pmull of setcr0*reg*reg*reg
  | Pdiv of setcr0*reg*reg*reg

(* For these cr0 seting is implicit *)
  | Paddi of reg*reg*k (* no *)
  | Pandi of reg*reg*k (* yes *)
  | Pori of reg*reg*k  (* no *)
  | Pxori of reg*reg*k (* no *)
  | Pmulli of  reg*reg*k (* no *)

  | Pli of reg*k
  | Pb of lbl
  | Pbcc of cond * lbl
  | Pcmpwi of crfindex * reg*k
  | Pcmpw of crfindex * reg*reg
  | Plwz of reg*idx*reg (* load 32-bit int; lwzx: same, with 2 index regs, hidden in addressing mode *)
  | Plwzx of reg*reg*reg
  | Pmr of reg * reg
  | Pstw of reg*idx*reg (* store 32-bit int; stwx: same, with 2 index regs, hidden in addressing mode *)
  | Pstwx of reg*reg*reg
  | Plwarx of reg*reg*reg (* load word and reserve indexed *)
  | Pstwcx of reg*reg*reg (* store word conditional indexed *)
(* 64bit load & store, needed by litmus, memevents will consider
   those as lwz/stw, so avoid in presented examples *)
  | Pstd of  reg*idx*reg
  | Pstdx of reg*reg*reg
  | Pld of  reg*idx*reg
  | Pldx of reg*reg*reg
(* Fence instructions *)
  | Psync
  | Peieio
  | Pisync
  | Plwsync
(* Extra, is a nop in memevents *)
  | Pdcbf of reg*reg
(* New acquire/release instructions *)
  | Plwzx_acq of reg * reg * reg
  | Pstwx_rel of reg * reg * reg

(*: ARM Instructions :*)

type setflags = SetFlags | DontSetFlags

type condition = NE | EQ | AL (* ALWAYS *)

type arm_instruction =
  | I_ADD of setflags * reg * reg * k
  | I_ADD3 of setflags * reg * reg * reg
  | I_SUB of setflags * reg * reg * k
  | I_SUB3 of setflags * reg * reg * reg
  | I_AND of setflags * reg * reg * k
  | I_B of lbl
  | I_BEQ of lbl 
  | I_BNE of lbl (* Was maybeVal ??? *)
  | I_CMPI of reg * k
  | I_CMP of reg * reg
  | I_LDR of reg * reg * condition
  | I_LDREX of reg * reg
  | I_STREX of reg * reg
  | I_LDR3 of reg * reg * reg * condition
  | I_STR of reg * reg * condition
  | I_STR3 of reg * reg * reg * condition
  | I_MOVI of reg * k * condition
  | I_MOV of reg * reg * condition
  | I_XOR of setflags * reg * reg * reg
  | I_DMB
  | I_DSB
  | I_ISB

type instruction = 
  | PPC_ins of ppc_instruction
  | ARM_ins of arm_instruction


(*: \subsubsection{Assembly instruction `abstract microcode' abstract syntax} :*)

(*: The state of a running instruction instance is a list of actions together with the current valuation for flexible variables. :*)

(* We later (in can_transition) suppose that there are no Read_reg, Read_mem, Binop, or Unop after any other action *)

type action = 
  | Read_reg of reg * value
  | Write_reg of reg * value
  | Read_mem of address * value
  | Write_mem of address * value
  | Read_mem_reserve of address * value
  | Write_mem_conditional of address * value * value (* If Wcond a v1 succeeds, set v2 *)
  | Read_mem_acq of address * value
  | Write_mem_rel of address * value
  | Binop of value * op * value * value
  | Unop of value * op1 * value
  | Barrier of barrier_type
  | Isync
  | Jump of cst
  | Cond_branch of value * cst

type sem_state =
    { remaining : action list;
     val_soln : solution
  }

type reaches_by =
  | Always
  | IfZero of value
  | IfNonZero of value

type next_instr =
    (* continue in sequence *)
  | Next
      (* jump to arg *)
  | Jump_to of cst
	(* if v is one, jump to address, otherwise continue in sequence *)
  | Cond_branch_to of value * cst


type instruction_instance =
       { ioid : ioid; (*: Chosen to make every instance unique :*)
          behaviour : sem_state;  (*: The current state of the instruction evaluation. This component
                                      evolves through time, as the instruction consumes values from
                                      register and memory reads, performs computations, and produces
                                      values for register or memory writes, or possibly makes requests for
                                      barrier operations. :*)
          regs_in : reg set;         (*: The input registers, for ease of dependency calculation :*)
          regs_out : reg set;        (*: The output registers, for ease of dependency calculation :*)

(*           is_memory_read : bool;     (\*: These are easy to calculate from the instruction and its initial behaviour. Remember for convenience :*\) *)
(*           is_memory_write : bool; *)
          is_load : bool;     (*: These are easy to calculate from the instruction and its initial behaviour. Remember for convenience :*)
          is_store : bool;
          is_load_reserve : bool;
          is_store_conditional : bool; 
          is_load_acquire : bool;
          is_store_release : bool; 

          is_isync : bool;
          is_lwsync : bool; 
          is_eieio : bool; 
          is_sync : bool;
          is_branch : bool;

          read_responses : read_response set; (*: Read responses :*)

          writes_read_from : write set;  (*: Tracking writes read from, to determine restart candidates at
                                          invalidates. This component starts out empty and evolves through time. :*)
          program_loc : address;     (*: record fetched address for convenience :*)
          instruction : instruction; (*: record actual instruction, for convenience :*)
          prev : (ioid * reaches_by)option (*: pointer to program-order-previous instruction instance, NONE for start :*)
    }

type thread_state = {
       thread : thread_id ;                                         (* the id of this thread, for reference *)
       initial_register_state : reg -> value ;                (* Map from registers to values *)
       committed_instructions : instruction_instance set;     (* instructions that have been committed *)
       in_flight_instructions : instruction_instance set;     (* instructions in flight *)
       unacknowledged_syncs : barrier set              (* Sync acknowledgements not yet received *)
    }

type coherence_commit_params = 
  | Partial_CC
  | Late_CC

type ss_params =
  { coherence_commit : coherence_commit_params;
     pcc_deadlock_avoid : bool;
     coherence_points : bool
  }


type thread_loose_tight_params =
  | Thread_loose
  | Thread_tight

type thread_lwsync_params =
  | Lwsync_read_restart
  | Lwsync_read_block

type thread_restart_forwarded_params =
  | Restart_forwarded_reads
  | Dont_restart_forwarded_reads

type thread_params =
 { thread_loose_tight : thread_loose_tight_params;
    thread_restart_forwarded : thread_restart_forwarded_params;
    thread_lwsync : thread_lwsync_params 
  }


type model_params =
  { ss : ss_params;
     t  : thread_params 
  } 


type system_state =
       { thread_states : thread_id -> thread_state;
          storage_subsystem : storage_subsystem_state;
	  id_state : id_state;
          model : model_params;
       }


(* transitions *)
type ss_trans =
   | SS_Accept_write_request of write
   | SS_Accept_successful_write_conditional_request of write_conditional_request
   | SS_Accept_failing_write_conditional_request of write_conditional_request
   | SS_Partial_coherence_commit of write * write
   | SS_Propagate_write_to_thread of write * thread_id
   | SS_Write_reaches_coherence_point of write
   | SS_Send_read_response of thread_id * read_request * write
   | SS_Accept_barrier_request of barrier
   | SS_Propagate_barrier_to_thread of barrier * thread_id
   | SS_Acknowledge_sync_barrier of barrier
 
type thread_trans =
   | T_Fetch of instruction_instance
   | T_Commit_write of instruction_instance * write set
   | T_Commit_write_conditional of instruction_instance * write_conditional_request * bool
   | T_Commit_barrier of instruction_instance * barrier set
   | T_Commit_read of instruction_instance * read_response set
   | T_Commit_reg_or_branch of instruction_instance
   | T_Accept_sync_ack of barrier_ack
   | T_Satisfy_read_from_storage of instruction_instance * read_request * write
   | T_Satisfy_read_from_forwarded_write of instruction_instance * write * instruction_instance
   | T_Satisfy_read_reserve of instruction_instance * read_request * write
   | T_Register_read_from_previous_write of instruction_instance * reg * value * instruction_instance
   | T_Register_read_from_initial of instruction_instance * reg * value
   | T_Partial_evaluate of instruction_instance
   | T_Restart of instruction_instance
   | T_Abort of instruction_instance

type trans =
   | Fetch of thread_id * instruction_instance
   | Commit_write of thread_id * instruction_instance * write set
   | Commit_barrier of thread_id * instruction_instance * barrier
   | Commit_write_conditional of thread_id * instruction_instance * write_conditional_request * bool
   | Commit_read of thread_id * instruction_instance * read_response set
   | Commit_reg_or_branch of thread_id * instruction_instance
   | Write_propagate_to_thread of write * thread_id
   | Barrier_propagate_to_thread of barrier * thread_id 
   | Read_from_storage_subsystem of thread_id * instruction_instance * write
   | Write_forward_to_read of thread_id * instruction_instance * write * instruction_instance
   | Acknowledge_sync of barrier
   | Partial_coherence_commit of write * write
   | Write_reaching_coherence_point of write
   | Register_read_prev of thread_id * instruction_instance * reg * instruction_instance
   | Register_read_initial of thread_id * instruction_instance * reg
   | Partial_evaluate of thread_id * instruction_instance
   | Restart_instruction_instance of thread_id * instruction_instance
   | Abort_instruction_instance of thread_id * instruction_instance