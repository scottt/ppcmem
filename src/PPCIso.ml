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

module Make(V:Value.S) : Iso.S with module A = PPCArch.Make(V) =
  struct
    module A = PPCArch.Make(V)

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

    let ireg_to_arch_ireg ir =
      match ir with
      | MDT.GPR0  -> A.GPR0
      | MDT.GPR1  -> A.GPR1
      | MDT.GPR2  -> A.GPR2
      | MDT.GPR3  -> A.GPR3
      | MDT.GPR4  -> A.GPR4
      | MDT.GPR5  -> A.GPR5
      | MDT.GPR6  -> A.GPR6
      | MDT.GPR7  -> A.GPR7
      | MDT.GPR8  -> A.GPR8
      | MDT.GPR9  -> A.GPR9
      | MDT.GPR10 -> A.GPR10
      | MDT.GPR11 -> A.GPR11
      | MDT.GPR12 -> A.GPR12
      | MDT.GPR13 -> A.GPR13
      | MDT.GPR14 -> A.GPR14
      | MDT.GPR15 -> A.GPR15
      | MDT.GPR16 -> A.GPR16
      | MDT.GPR17 -> A.GPR17
      | MDT.GPR18 -> A.GPR18
      | MDT.GPR19 -> A.GPR19
      | MDT.GPR20 -> A.GPR20
      | MDT.GPR21 -> A.GPR21
      | MDT.GPR22 -> A.GPR22
      | MDT.GPR23 -> A.GPR23
      | MDT.GPR24 -> A.GPR24
      | MDT.GPR25 -> A.GPR25
      | MDT.GPR26 -> A.GPR26
      | MDT.GPR27 -> A.GPR27
      | MDT.GPR28 -> A.GPR28
      | MDT.GPR29 -> A.GPR29
      | MDT.GPR30 -> A.GPR30
      | MDT.GPR31 -> A.GPR31

    let arch_ireg_to_ireg ir =
      match ir with
      | A.GPR0  -> MDT.GPR0
      | A.GPR1  -> MDT.GPR1
      | A.GPR2  -> MDT.GPR2
      | A.GPR3  -> MDT.GPR3
      | A.GPR4  -> MDT.GPR4
      | A.GPR5  -> MDT.GPR5
      | A.GPR6  -> MDT.GPR6
      | A.GPR7  -> MDT.GPR7
      | A.GPR8  -> MDT.GPR8
      | A.GPR9  -> MDT.GPR9
      | A.GPR10 -> MDT.GPR10
      | A.GPR11 -> MDT.GPR11
      | A.GPR12 -> MDT.GPR12
      | A.GPR13 -> MDT.GPR13
      | A.GPR14 -> MDT.GPR14
      | A.GPR15 -> MDT.GPR15
      | A.GPR16 -> MDT.GPR16
      | A.GPR17 -> MDT.GPR17
      | A.GPR18 -> MDT.GPR18
      | A.GPR19 -> MDT.GPR19
      | A.GPR20 -> MDT.GPR20
      | A.GPR21 -> MDT.GPR21
      | A.GPR22 -> MDT.GPR22
      | A.GPR23 -> MDT.GPR23
      | A.GPR24 -> MDT.GPR24
      | A.GPR25 -> MDT.GPR25
      | A.GPR26 -> MDT.GPR26
      | A.GPR27 -> MDT.GPR27
      | A.GPR28 -> MDT.GPR28
      | A.GPR29 -> MDT.GPR29
      | A.GPR30 -> MDT.GPR30
      | A.GPR31 -> MDT.GPR31

    let reg_to_arch_reg r = 
      match r with
      | MDT.PPC_reg (MDT.Ireg ir) -> A.Ireg (ireg_to_arch_ireg ir)
      | MDT.PPC_reg (MDT.PC)      -> A.PC
      | MDT.PPC_reg (MDT.CRBit n) -> A.CRBit n
      | MDT.ARM_reg _ -> assert(false)

    let arch_reg_to_reg r = 
      match r with
      | A.Ireg ir -> MDT.PPC_reg (MDT.Ireg (arch_ireg_to_ireg ir))
      | A.PC      -> MDT.PPC_reg (MDT.PC)
      | A.CRBit n -> MDT.PPC_reg (MDT.CRBit n)

(*      | _ -> Warn.user_error "Cannot handle register %s" (A.pp_reg r)*)

    let cond_to_arch_cond c =
      match c with
      | MDT.Eq -> A.Eq
      | MDT.Ne -> A.Ne
      | MDT.Lt -> A.Lt
      | MDT.Ge -> A.Ge
      | MDT.Gt -> A.Gt
      | MDT.Le -> A.Le

    let arch_cond_to_cond c =
      match c with
      | A.Eq -> MDT.Eq
      | A.Ne -> MDT.Ne
      | A.Lt -> MDT.Lt
      | A.Ge -> MDT.Ge
      | A.Gt -> MDT.Gt
      | A.Le -> MDT.Le

    let setcr0_to_arch_setcr0 c =
      match c with
      | MDT.SetCR0 -> A.SetCR0
      | MDT.DontSetCR0 -> A.DontSetCR0

    let arch_setcr0_to_setcr0 c =
      match c with
      | A.SetCR0 -> MDT.SetCR0
      | A.DontSetCR0 -> MDT.DontSetCR0

    let instr_to_arch_instr i = 
      match i with 
      | MDT.ARM_ins _ -> assert(false)
      | MDT.PPC_ins i ->
          match i with
          | MDT.Padd (s,r1,r2,r3) -> 
	      A.Padd (setcr0_to_arch_setcr0 s,
		      reg_to_arch_reg r1,
		      reg_to_arch_reg r2,
		      reg_to_arch_reg r3)
          | MDT.Psub (s,r1,r2,r3) -> 
	      A.Psub (setcr0_to_arch_setcr0 s,
		      reg_to_arch_reg r1,
		      reg_to_arch_reg r2,
		      reg_to_arch_reg r3)
          | MDT.Por (s,r1,r2,r3) -> 
	      A.Por (setcr0_to_arch_setcr0 s,
		     reg_to_arch_reg r1,
		     reg_to_arch_reg r2,
		     reg_to_arch_reg r3)
          | MDT.Pand (s,r1,r2,r3) -> 
	      A.Pand (setcr0_to_arch_setcr0 s,
		      reg_to_arch_reg r1,
		      reg_to_arch_reg r2,
		      reg_to_arch_reg r3)
          | MDT.Pxor (s,r1,r2,r3) -> 
	      A.Pxor (setcr0_to_arch_setcr0 s,
		      reg_to_arch_reg r1,
		      reg_to_arch_reg r2,
		      reg_to_arch_reg r3)
          | MDT.Pmull (s,r1,r2,r3) -> 
	      A.Pmull (setcr0_to_arch_setcr0 s,
		       reg_to_arch_reg r1,
		       reg_to_arch_reg r2,
		       reg_to_arch_reg r3)
          | MDT.Pdiv (s,r1,r2,r3) -> 
	      A.Pdiv (setcr0_to_arch_setcr0 s,
		      reg_to_arch_reg r1,
		      reg_to_arch_reg r2,
		      reg_to_arch_reg r3)
          | MDT.Paddi (r1,r2,k) -> 
	      A.Paddi (reg_to_arch_reg r1,
		       reg_to_arch_reg r2,
		       k)
          | MDT.Pandi (r1,r2,k) -> 
	      A.Pandi (reg_to_arch_reg r1,
		       reg_to_arch_reg r2,
		       k)
          | MDT.Pori (r1,r2,k) -> 
	      A.Pori (reg_to_arch_reg r1,
		      reg_to_arch_reg r2,
		      k)
          | MDT.Pxori (r1,r2,k) -> 
	      A.Pxori (reg_to_arch_reg r1,
		       reg_to_arch_reg r2,
		       k)
          | MDT.Pmulli (r1,r2,k) -> 
	      A.Pmulli (reg_to_arch_reg r1,
		        reg_to_arch_reg r2,
		        k)
          | MDT.Pli (r,k) ->
	      A.Pli (reg_to_arch_reg r,
		     k)
          | MDT.Pb l -> A.Pb l
          | MDT.Pbcc (c,l) ->
	      A.Pbcc (cond_to_arch_cond c,
		      l)
          | MDT.Pcmpwi (c,r,k) ->
	      A.Pcmpwi (c,reg_to_arch_reg r,k)
          | MDT.Pcmpw (c,r1,r2) ->
	      A.Pcmpw (c,reg_to_arch_reg r1,reg_to_arch_reg r2)
          | MDT.Plwz (r1,i,r2) ->
	      A.Plwz (reg_to_arch_reg r1,
		      i,
		      reg_to_arch_reg r2)
          | MDT.Plwzx (r1,r2,r3) ->
	      A.Plwzx (reg_to_arch_reg r1,
		       reg_to_arch_reg r2,
		       reg_to_arch_reg r3)
          | MDT.Pmr (r1,r2) ->
	      A.Pmr (reg_to_arch_reg r1,
		     reg_to_arch_reg r2)
          | MDT.Pstw (r1,i,r2) ->
	      A.Pstw (reg_to_arch_reg r1,
		      i,
		      reg_to_arch_reg r2)
          | MDT.Pstwx (r1,r2,r3) ->
	      A.Pstwx (reg_to_arch_reg r1,
		       reg_to_arch_reg r2,
		       reg_to_arch_reg r3)
          | MDT.Plwarx (r1,r2,r3) ->
	      A.Plwarx (reg_to_arch_reg r1,
		        reg_to_arch_reg r2,
		        reg_to_arch_reg r3)
          | MDT.Pstwcx (r1,r2,r3) ->
	      A.Pstwcx (reg_to_arch_reg r1,
		        reg_to_arch_reg r2,
		        reg_to_arch_reg r3)
          | MDT.Pstd (r1,i,r2) ->
	      A.Pstd (reg_to_arch_reg r1,
		      i,
		      reg_to_arch_reg r2)
          | MDT.Pstdx (r1,r2,r3) ->
	      A.Pstdx (reg_to_arch_reg r1,
		       reg_to_arch_reg r2,
		       reg_to_arch_reg r3)
          | MDT.Pld (r1,i,r2) ->
	      A.Pld (reg_to_arch_reg r1,
		     i,
		     reg_to_arch_reg r2)
          | MDT.Pldx (r1,r2,r3) ->
	      A.Pldx (reg_to_arch_reg r1,
		      reg_to_arch_reg r2,
		      reg_to_arch_reg r3)
          | MDT.Psync -> A.Psync
          | MDT.Peieio -> A.Peieio
          | MDT.Pisync -> A.Pisync
          | MDT.Plwsync -> A.Plwsync
          | MDT.Pdcbf (r1,r2) ->
	      A.Pdcbf (reg_to_arch_reg r1,
		       reg_to_arch_reg r2)

    let arch_instr_to_instr0 i = 
      match i with
      | A.Padd (s,r1,r2,r3) -> 
	  MDT.Padd (arch_setcr0_to_setcr0 s,
		  arch_reg_to_reg r1,
		  arch_reg_to_reg r2,
		  arch_reg_to_reg r3)
      | A.Psub (s,r1,r2,r3) -> 
	  MDT.Psub (arch_setcr0_to_setcr0 s,
		  arch_reg_to_reg r1,
		  arch_reg_to_reg r2,
		  arch_reg_to_reg r3)
      | A.Por (s,r1,r2,r3) -> 
	  MDT.Por (arch_setcr0_to_setcr0 s,
		  arch_reg_to_reg r1,
		  arch_reg_to_reg r2,
		  arch_reg_to_reg r3)
      | A.Pand (s,r1,r2,r3) -> 
	  MDT.Pand (arch_setcr0_to_setcr0 s,
		  arch_reg_to_reg r1,
		  arch_reg_to_reg r2,
		  arch_reg_to_reg r3)
      | A.Pxor (s,r1,r2,r3) -> 
	  MDT.Pxor (arch_setcr0_to_setcr0 s,
		  arch_reg_to_reg r1,
		  arch_reg_to_reg r2,
		  arch_reg_to_reg r3)
      | A.Pmull (s,r1,r2,r3) -> 
	  MDT.Pmull (arch_setcr0_to_setcr0 s,
		  arch_reg_to_reg r1,
		  arch_reg_to_reg r2,
		  arch_reg_to_reg r3)
      | A.Pdiv (s,r1,r2,r3) -> 
	  MDT.Pdiv (arch_setcr0_to_setcr0 s,
		  arch_reg_to_reg r1,
		  arch_reg_to_reg r2,
		  arch_reg_to_reg r3)
      | A.Paddi (r1,r2,k) -> 
	  MDT.Paddi (arch_reg_to_reg r1,
		   arch_reg_to_reg r2,
		   k)
      | A.Pandi (r1,r2,k) -> 
	  MDT.Pandi (arch_reg_to_reg r1,
		   arch_reg_to_reg r2,
		   k)
      | A.Pori (r1,r2,k) -> 
	  MDT.Pori (arch_reg_to_reg r1,
		   arch_reg_to_reg r2,
		   k)
      | A.Pxori (r1,r2,k) -> 
	  MDT.Pxori (arch_reg_to_reg r1,
		   arch_reg_to_reg r2,
		   k)
      | A.Pmulli (r1,r2,k) -> 
	  MDT.Pmulli (arch_reg_to_reg r1,
		   arch_reg_to_reg r2,
		   k)
      | A.Pli (r,k) ->
	  MDT.Pli (arch_reg_to_reg r,
		 k)
      | A.Pb l -> MDT.Pb l
      | A.Pbcc (c,l) ->
	  MDT.Pbcc (arch_cond_to_cond c,
		  l)
      | A.Pcmpwi (c,r,k) ->
	  MDT.Pcmpwi (c,arch_reg_to_reg r,k)
      | A.Pcmpw (c,r1,r2) ->
	  MDT.Pcmpw (c,arch_reg_to_reg r1,arch_reg_to_reg r2)
      | A.Plwz (r1,i,r2) ->
	  MDT.Plwz (arch_reg_to_reg r1,
		  i,
		  arch_reg_to_reg r2)
      | A.Plwzx (r1,r2,r3) ->
	  MDT.Plwzx (arch_reg_to_reg r1,
		   arch_reg_to_reg r2,
		   arch_reg_to_reg r3)
      | A.Pmr (r1,r2) ->
	  MDT.Pmr (arch_reg_to_reg r1,
		   arch_reg_to_reg r2)
      | A.Pstw (r1,i,r2) ->
	  MDT.Pstw (arch_reg_to_reg r1,
		  i,
		  arch_reg_to_reg r2)
      | A.Pstwx (r1,r2,r3) ->
	  MDT.Pstwx (arch_reg_to_reg r1,
		   arch_reg_to_reg r2,
		   arch_reg_to_reg r3)
      | A.Plwarx (r1,r2,r3) ->
	  MDT.Plwarx (arch_reg_to_reg r1,
		   arch_reg_to_reg r2,
		   arch_reg_to_reg r3)
      | A.Pstwcx (r1,r2,r3) ->
	  MDT.Pstwcx (arch_reg_to_reg r1,
		   arch_reg_to_reg r2,
		   arch_reg_to_reg r3)
      | A.Pstd (r1,i,r2) ->
	  MDT.Pstd (arch_reg_to_reg r1,
		  i,
		  arch_reg_to_reg r2)
      | A.Pstdx (r1,r2,r3) ->
	  MDT.Pstdx (arch_reg_to_reg r1,
		   arch_reg_to_reg r2,
		   arch_reg_to_reg r3)
      | A.Pld (r1,i,r2) ->
	  MDT.Pld (arch_reg_to_reg r1,
		  i,
		  arch_reg_to_reg r2)
      | A.Pldx (r1,r2,r3) ->
	  MDT.Pldx (arch_reg_to_reg r1,
		   arch_reg_to_reg r2,
		   arch_reg_to_reg r3)
      | A.Psync -> MDT.Psync
      | A.Peieio -> MDT.Peieio
      | A.Pisync -> MDT.Pisync
      | A.Plwsync -> MDT.Plwsync
      | A.Pdcbf (r1,r2) ->
	  MDT.Pdcbf (arch_reg_to_reg r1,
		   arch_reg_to_reg r2)

    let arch_instr_to_instr i = MDT.PPC_ins (arch_instr_to_instr0 i)

  end
