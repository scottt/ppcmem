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

module type S = 
  sig 
    module A : Arch.S

    val arch_const_to_const : A.V.cst -> MachineDefValue.cst
    val const_to_arch_const : MachineDefValue.cst -> A.V.cst

    val arch_value_to_value : A.V.v -> MachineDefValue.value
    val value_to_arch_value : MachineDefValue.value -> A.V.v

    val arch_proc_to_thread_id : A.proc -> MachineDefTypes.thread_id
    val thread_id_to_arch_proc : MachineDefTypes.thread_id -> A.proc

    val arch_reg_to_reg : A.reg -> MachineDefTypes.reg
    val reg_to_arch_reg : MachineDefTypes.reg -> A.reg

    val arch_instr_to_instr : A.instruction -> MachineDefTypes.instruction
    val instr_to_arch_instr : MachineDefTypes.instruction -> A.instruction
  end
