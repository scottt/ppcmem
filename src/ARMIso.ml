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

module Make(V:Value.S) : Iso.S with module A = ARMArch.Make(V) =
  struct
    module A = ARMArch.Make(V)

    module MDV = MachineDefValue
    module MDT = MachineDefTypes

    let const_to_arch_const c = 
      match c with
      | MDV.Concrete n -> SymbConstant.Concrete n
      | MDV.Symbolic s -> SymbConstant.Symbolic s

    let arch_const_to_const c = 
      match c with
      | SymbConstant.Concrete n -> MDV.Concrete n
      | SymbConstant.Symbolic s -> MDV.Symbolic s

    let value_to_arch_value v = 
      match v with
      | MDV.Rigid c -> A.V.Val (const_to_arch_const c)
      | MDV.Flexible v -> A.V.Var v

    let arch_value_to_value v = 
      match v with
      | A.V.Val c -> MDV.Rigid (arch_const_to_const c)
      | A.V.Var v -> MDV.Flexible v 

    let thread_id_to_arch_proc t = t
    let arch_proc_to_thread_id t = t

    let arm_reg_to_arch_arm_reg r =
      match r with
      | MDT.R0   -> A.R0  
      | MDT.R1   -> A.R1  
      | MDT.R2   -> A.R2  
      | MDT.R3   -> A.R3  
      | MDT.R4   -> A.R4  
      | MDT.R5   -> A.R5  
      | MDT.R6   -> A.R6  
      | MDT.R7   -> A.R7  
      | MDT.R8   -> A.R8  
      | MDT.R9   -> A.R9  
      | MDT.R10  -> A.R10 
      | MDT.R11  -> A.R11 
      | MDT.R12  -> A.R12 
      | MDT.SP   -> A.SP  
      | MDT.LR   -> A.LR  
      | MDT.ARM_PC   -> A.PC  
      | MDT.Z    -> A.Z

    let arch_arm_reg_to_arm_reg r =
      match r with
      | A.R0   -> MDT.R0  
      | A.R1   -> MDT.R1  
      | A.R2   -> MDT.R2  
      | A.R3   -> MDT.R3  
      | A.R4   -> MDT.R4  
      | A.R5   -> MDT.R5  
      | A.R6   -> MDT.R6  
      | A.R7   -> MDT.R7  
      | A.R8   -> MDT.R8  
      | A.R9   -> MDT.R9  
      | A.R10  -> MDT.R10 
      | A.R11  -> MDT.R11 
      | A.R12  -> MDT.R12 
      | A.SP   -> MDT.SP  
      | A.LR   -> MDT.LR  
      | A.PC   -> MDT.ARM_PC  
      | A.Z    -> MDT.Z

    let reg_to_arch_reg r = 
      match r with
      | MDT.ARM_reg r -> arm_reg_to_arch_arm_reg r
      | MDT.PPC_reg _ -> assert(false)

    let arch_reg_to_reg r = 
       MDT.ARM_reg (arch_arm_reg_to_arm_reg r)

(*      | _ -> Warn.user_error "Cannot handle register %s" (A.pp_reg r)*)

    let condition_to_arch_condition c =
      match c with
      | MDT.EQ -> A.EQ
      | MDT.NE -> A.NE
      | MDT.AL -> A.AL

    let arch_condition_to_condition c =
      match c with
      | A.EQ -> MDT.EQ
      | A.NE -> MDT.NE
      | A.AL -> MDT.AL

    let setflags_to_arch_setflags c =
      match c with
      | MDT.SetFlags -> A.SetFlags
      | MDT.DontSetFlags -> A.DontSetFlags

    let arch_setflags_to_setflags c =
      match c with
      | A.SetFlags -> MDT.SetFlags
      | A.DontSetFlags -> MDT.DontSetFlags

    let instr_to_arch_instr i = 
      match i with 
      | MDT.PPC_ins _ -> assert(false)
      | MDT.ARM_ins i ->
          match i with
          | MDT.I_ADD3 (s,r1,r2,r3) -> 
	      A.I_ADD3 (setflags_to_arch_setflags s,
		        reg_to_arch_reg r1,
		        reg_to_arch_reg r2,
		        reg_to_arch_reg r3)
          | MDT.I_SUB3 (s,r1,r2,r3) -> 
	      A.I_SUB3 (setflags_to_arch_setflags s,
		        reg_to_arch_reg r1,
		        reg_to_arch_reg r2,
		        reg_to_arch_reg r3)
          | MDT.I_XOR (s,r1,r2,r3) -> 
	      A.I_XOR (setflags_to_arch_setflags s,
		       reg_to_arch_reg r1,
		       reg_to_arch_reg r2,
		       reg_to_arch_reg r3)
          | MDT.I_ADD (s,r1,r2,k) -> 
	      A.I_ADD (setflags_to_arch_setflags s,
                       reg_to_arch_reg r1,
		       reg_to_arch_reg r2,
		       k)
          | MDT.I_SUB (s,r1,r2,k) -> 
	      A.I_SUB (setflags_to_arch_setflags s,
                       reg_to_arch_reg r1,
		       reg_to_arch_reg r2,
		       k)
          | MDT.I_AND (s,r1,r2,k) -> 
	      A.I_AND (setflags_to_arch_setflags s,
                       reg_to_arch_reg r1,
		       reg_to_arch_reg r2,
		       k)
          | MDT.I_MOVI (r,k,c) ->
	      A.I_MOVI (reg_to_arch_reg r,
		        k,
                        condition_to_arch_condition c)
          | MDT.I_B l -> A.I_B l
          | MDT.I_BEQ l -> A.I_BEQ l
          | MDT.I_BNE l -> A.I_BNE l
          | MDT.I_CMPI (r,k) ->
	      A.I_CMPI (reg_to_arch_reg r,k)
          | MDT.I_CMP (r1,r2) ->
	      A.I_CMP (reg_to_arch_reg r1,reg_to_arch_reg r2)
          | MDT.I_LDR (r1,r2,c) ->
	      A.I_LDR (reg_to_arch_reg r1,
		       reg_to_arch_reg r2,
                       condition_to_arch_condition c)
          | MDT.I_LDR3 (r1,r2,r3,c) ->
	      A.I_LDR3 (reg_to_arch_reg r1,
		        reg_to_arch_reg r2,
		        reg_to_arch_reg r3,
                        condition_to_arch_condition c)
          | MDT.I_LDREX (r1,r2) ->
	      A.I_LDREX (reg_to_arch_reg r1,
		       reg_to_arch_reg r2)
          | MDT.I_STR (r1,r2,c) ->
	      A.I_STR (reg_to_arch_reg r1,
		       reg_to_arch_reg r2,
                       condition_to_arch_condition c)
          | MDT.I_STR3 (r1,r2,r3,c) ->
	      A.I_STR3 (reg_to_arch_reg r1,
		        reg_to_arch_reg r2,
		        reg_to_arch_reg r3,
                        condition_to_arch_condition c)
(*           | MDT.I_STREX (r1,r2) -> *)
(* 	      A.I_STR (reg_to_arch_reg r1, *)
(* 		       reg_to_arch_reg r2) *)
          | MDT.I_MOV (r1,r2,c) ->
	      A.I_MOV (reg_to_arch_reg r1,
		       reg_to_arch_reg r2,
                       condition_to_arch_condition c)
          | MDT.I_DSB -> A.I_DSB
          | MDT.I_DMB -> A.I_DMB
          | MDT.I_ISB -> A.I_ISB

    let arch_instr_to_instr0 i = 
      match i with
      | A.I_ADD3 (s,r1,r2,r3) -> 
	  MDT.I_ADD3 (arch_setflags_to_setflags s,
		      arch_reg_to_reg r1,
		      arch_reg_to_reg r2,
		      arch_reg_to_reg r3)
      | A.I_SUB3 (s,r1,r2,r3) -> 
	  MDT.I_SUB3 (arch_setflags_to_setflags s,
		      arch_reg_to_reg r1,
		      arch_reg_to_reg r2,
		      arch_reg_to_reg r3)
      | A.I_XOR (s,r1,r2,r3) -> 
	  MDT.I_XOR (arch_setflags_to_setflags s,
		     arch_reg_to_reg r1,
		     arch_reg_to_reg r2,
		     arch_reg_to_reg r3)
      | A.I_ADD (s,r1,r2,k) -> 
	  MDT.I_ADD (arch_setflags_to_setflags s,
                     arch_reg_to_reg r1,
		     arch_reg_to_reg r2,
		     k)
      | A.I_SUB (s,r1,r2,k) -> 
	  MDT.I_SUB (arch_setflags_to_setflags s,
                     arch_reg_to_reg r1,
		     arch_reg_to_reg r2,
		     k)
      | A.I_AND (s,r1,r2,k) -> 
	  MDT.I_AND (arch_setflags_to_setflags s,
                     arch_reg_to_reg r1,
		     arch_reg_to_reg r2,
		     k)
      | A.I_MOVI (r,k,c) ->
	  MDT.I_MOVI (arch_reg_to_reg r,
		      k,
                      arch_condition_to_condition c)
      | A.I_B l -> MDT.I_B l
      | A.I_BEQ l -> MDT.I_BEQ l
      | A.I_BNE l -> MDT.I_BNE l
      | A.I_CMPI (r,k) ->
	  MDT.I_CMPI (arch_reg_to_reg r,k)
      | A.I_CMP (r1,r2) ->
	  MDT.I_CMP (arch_reg_to_reg r1,arch_reg_to_reg r2)
      | A.I_LDR (r1,r2,c) ->
	  MDT.I_LDR (arch_reg_to_reg r1,
		     arch_reg_to_reg r2,
                     arch_condition_to_condition c)
      | A.I_LDR3 (r1,r2,r3,c) ->
	  MDT.I_LDR3 (arch_reg_to_reg r1,
		      arch_reg_to_reg r2,
		      arch_reg_to_reg r3,
                      arch_condition_to_condition c)
      | A.I_LDREX (r1,r2) ->
	  MDT.I_LDREX (arch_reg_to_reg r1,
		     arch_reg_to_reg r2)
      | A.I_STR (r1,r2,c) ->
	  MDT.I_STR (arch_reg_to_reg r1,
		     arch_reg_to_reg r2,
                     arch_condition_to_condition c)
      | A.I_STR3 (r1,r2,r3,c) ->
	  MDT.I_STR3 (arch_reg_to_reg r1,
		      arch_reg_to_reg r2,
		      arch_reg_to_reg r3,
                      arch_condition_to_condition c)
(*           | A.I_STREX (r1,r2) -> *)
(* 	      MDT.I_STR (arch_reg_to_reg r1, *)
(* 		       arch_reg_to_reg r2) *)
      | A.I_MOV (r1,r2,c) ->
	  MDT.I_MOV (arch_reg_to_reg r1,
		     arch_reg_to_reg r2,
                     arch_condition_to_condition c)
      | A.I_DSB -> MDT.I_DSB
      | A.I_DMB -> MDT.I_DMB
      | A.I_ISB -> MDT.I_ISB

    let arch_instr_to_instr i = MDT.ARM_ins (arch_instr_to_instr0 i)

  end
