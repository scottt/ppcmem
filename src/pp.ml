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

(* module Make (SEM:Semantics.S) =
  struct *)

open Printf

open MachineDefUtils
open MachineDefValue
open MachineDefTypes
open MachineDefInstructionSemantics
open Types
open Model_aux

open Globals

(* vt220 colour definitions ************************************* *)
      
let black   = 0
let red     = 1
let green   = 2
let yellow  = 3
let blue    = 4
let magenta = 5
let cyan    = 6
let white   = 7
    
let _reset = "\x1b[0m"
let _color fg br = Printf.sprintf "\x1b[%u;%um" br (fg+30)
let _w = "\x1b[0;1;4m"
let _r = _color red 0
let _b = _color blue 0  (* was blue 1 *)
let _g = _color green 0

let col_wrap col s = col ^ s ^ _reset
                                 
let col_red  s =  col_wrap  _r s
let col_black  s = col_wrap  _b s
let col_green  s = col_wrap  _g s
let col_yellow  s = col_wrap  (_color yellow 0) s
let col_blue  s = col_wrap  (_color blue 0)  s
let col_magenta  s = col_wrap  (_color  magenta 0) s
let col_cyan  s = col_wrap  (_color  cyan 0) s 
let col_white  s = col_wrap   (_color  white 0)  s


let linebreak = ref "\n"
let linebreak_init () =  
	match ((Globals.get_our_runopts()).Globals.ppkind) with
		| Ascii | Latex -> linebreak := "\n"
		| Html -> linebreak :=  "<br/>"

let colour_old_new m c s =
  match c with
  | UI_removed -> ""
  | UI_old -> s
  | UI_new -> 
      if m.Globals.pp_colours then 
        match m.Globals.pp_kind with
        | Ascii -> col_red s  
		| Html -> "<fontcolor='red'>"^ s ^"</font>"
        | Latex -> "\\myred{" ^ s ^"}"
      else s

let colour_tran_id m s = 
  if m.Globals.pp_colours then 
    match m.Globals.pp_kind with
    | Ascii -> col_green s  
	| Html -> "<fontcolor='green'>"^ s ^"</font>"
    | Latex -> "\\mygreen{" ^ s ^"}"
  else s

let colour_memory_action m s = 
  if m.Globals.pp_colours then 
    match m.Globals.pp_kind with
    | Ascii -> col_cyan s  
	| Html -> "<fontcolor='cyan'>"^ s ^"</font>"
    | Latex -> "\\mycyan{" ^ s ^"}"
  else s
      


(* pp of instructions ****************************************** *)

let iregs =
  [
   GPR0,"r0";  GPR1,"r1";
   GPR2,"r2";  GPR3,"r3";
   GPR4,"r4";  GPR5,"r5";
   GPR6,"r6";  GPR7,"r7";
   GPR8,"r8";  GPR9,"r9";
   GPR10,"r10";  GPR11,"r11";
   GPR12,"r12";  GPR13,"r13";
   GPR14,"r14";  GPR15,"r15";
   GPR16,"r16";  GPR17,"r17";
   GPR18,"r18";  GPR19,"r19";
   GPR20,"r20";  GPR21,"r21";
   GPR22,"r22";  GPR23,"r23";
   GPR24,"r24";  GPR25,"r25";
   GPR26,"r26";  GPR27,"r27";
   GPR28,"r28";  GPR29,"r29";
   GPR30,"r30";  GPR31,"r31";
 ]

let pp_ireg r =
  try List.assoc r iregs with
  | Not_found -> assert false

let pp_ppc_reg r =
  match r with
  | Ireg ir -> pp_ireg ir
  | CRBit k -> sprintf "CR:%i" k
  | PC -> "PC"

let arm_regs =
  [
   R0, "R0" ;
   R1, "R1" ;
   R2, "R2" ;
   R3, "R3" ;
   R4, "R4" ;
   R5, "R5" ;
   R6, "R6" ;
   R7, "R7" ;
   R8, "R8" ;
   R9, "R9" ;
   R10, "R10" ;
   R11, "R11" ;
   R12, "R12" ;
   SP, "SP" ;
   LR, "LR" ;
   ARM_PC, "PC" ;
   Z, "Z" ;
 ]
    
let pp_arm_reg r = 
  try List.assoc r arm_regs with Not_found -> assert false

let pp_reg r = 
  match r with
  | PPC_reg r' -> pp_ppc_reg r'
  | ARM_reg r' -> pp_arm_reg r'

let pp_crf crb = sprintf "cr%i" crb

let pp_op o = 
  match o with
  | Add -> "+"
  | Sub -> "-"
  | Mul -> "*"
  | Div -> "/"
  | And -> "&"
  | Or -> "|"
  | Xor -> "^" (* in C ?? *)
  | EqOp -> "=="
  | LtOp -> "<"
  | GtOp -> ">"

let pp_op1 o = match o with
| Not -> "!"

let pp_k = string_of_int
let pp_idx = string_of_int

let ppi_index_mode opcode r1 r2 r3 =
  opcode^" "^pp_reg r1 ^ ","^pp_reg r2 ^ ","^pp_reg r3

let ppi_imm_index_mode opcode r1 d r2 =
  opcode^" "^pp_reg r1 ^ ","^pp_idx d ^ "("^pp_reg r2^")"

let ppi_imm_instr opcode r1 r2 v =
  opcode^" "^pp_reg r1 ^ ","^pp_reg r2 ^ ","^pp_k v

let ppi_ri opcode rD v = opcode^" "^pp_reg rD ^ ","^pp_k v

let ppi_rr opcode rD rS = opcode^" "^pp_reg rD^","^pp_reg rS

let pp_op3 memo set rD rA rB =
  let memo = match set with
  | SetCR0 -> memo ^ "."
  | DontSetCR0 -> memo in
  ppi_index_mode memo rD rA rB

let pp_cond cond = match cond with
| Eq -> "eq" | Ne -> "ne"
| Lt -> "lt" | Ge -> "ge"
| Gt -> "gt" | Le -> "le"

let pp_ppc_instruction i = match i with
| Padd(set,rD,rA,rB) -> pp_op3 "add" set rD rA rB
| Psub(set,rD,rA,rB) -> pp_op3 "subf" set rD rA rB
| Por(set,rD,rA,rB) -> pp_op3 "or" set rD rA rB
| Pxor(set,rD,rA,rB) -> pp_op3 "xor" set rD rA rB
| Pand(set,rD,rA,rB) -> pp_op3 "and" set rD rA rB
| Pmull(set,rD,rA,rB) -> pp_op3 "mullw" set rD rA rB
| Pdiv(set,rD,rA,rB) -> pp_op3 "divw" set rD rA rB

| Paddi(rD,rA,simm) -> ppi_imm_instr "addi" rD rA simm
| Pori(rD,rA,simm) -> ppi_imm_instr "ori" rD rA simm
| Pxori(rD,rA,simm) -> ppi_imm_instr "xori" rD rA simm
| Pandi(rD,rA,simm) -> ppi_imm_instr "andi." rD rA simm
| Pmulli(rD,rA,simm) -> ppi_imm_instr "mulli" rD rA simm

| Pli(rD,v) -> ppi_ri "li" rD v
| Pcmpwi (0,rS,v) -> ppi_ri "cmpwi" rS v
| Pcmpwi (crf,rS,v) ->
    "cmpwi" ^ " " ^pp_crf crf ^ "," ^ pp_reg rS  ^ "," ^ pp_k v
| Pb lbl -> "b   " ^ lbl
| Pbcc(cond, lbl) -> "b"^pp_cond cond ^ "  " ^ lbl 
| Pcmpw(0,rA,rB) -> ppi_rr "cmpw" rA rB
| Pcmpw(crf,rA,rB) ->
    "cmpw" ^ " " ^pp_crf crf ^ "," ^ pp_reg rA  ^ "," ^ pp_reg rB
| Plwz(rD,d,rA) -> ppi_imm_index_mode "lwz" rD d rA
| Plwzx(rD,rA,rB) -> ppi_index_mode "lwzx" rD rA rB
| Pmr (rD,rS) -> ppi_rr "mr" rD rS
| Pstw(rS,d,rA) -> ppi_imm_index_mode "stw" rS d rA
| Pstwx(rS,rA,rB) -> ppi_index_mode "stwx" rS rA rB
| Plwarx(rD,rA,rB) -> ppi_index_mode "lwarx" rD rA rB
| Pstwcx(rS,rA,rB) -> ppi_index_mode "stwcx." rS rA rB

| Pld(rD,d,rA) -> ppi_imm_index_mode "ld" rD d rA
| Pldx(rD,rA,rB) -> ppi_index_mode "ldx" rD rA rB
| Pstd(rS,d,rA) -> ppi_imm_index_mode "std" rS d rA
| Pstdx(rS,rA,rB) -> ppi_index_mode "stdx" rS rA rB

| Psync -> "sync"
| Plwsync -> "lwsync"
| Pisync -> "isync"
| Peieio -> "eieio"
| Pdcbf (r1,r2) -> ppi_rr "dcbf" r1 r2




let pp_hash = "#"
(* m = match m with *)
(* | Ascii | Dot -> "#" *)
(* | Latex -> "\\#" *)
(* | DotFig -> "\\\\#" *)

let pp_k_arm v = pp_hash ^ string_of_int v

type basic_pp = { pp_k : k -> string; }

let pp_memo memo = function
  | SetFlags -> memo ^ "S"
  | DontSetFlags -> memo

let pp_condition = function
  | NE -> "NE"
  | EQ -> "EQ"
  | AL -> ""

let pp_lbl lbl = lbl

let pp_memoc memo c = sprintf "%s%s" memo (pp_condition c)

let rec do_pp_instruction m = 
  let ppi_rrr opcode s rt rn rm =
    pp_memo opcode s^" "^ pp_reg rt ^ "," ^ pp_reg rn ^ "," ^ pp_reg rm in
  let ppi_rri opcode s rt rn v =
     pp_memo opcode s^" "^pp_reg rt ^ ", "^ pp_reg rn ^ ", " ^ m.pp_k v in
(*  let _ppi_rrv opcode rt rn v =
    opcode^" "^pp_reg rt ^ ", "^ pp_reg rn ^ ", " ^ pp_abs v in
*)
  let ppi_rrmc opcode rt rn c=
    pp_memoc opcode c^" "^pp_reg rt ^
    ", "^ "[" ^ pp_reg rn ^ "]" in
  let ppi_rrm opcode rt rn =  ppi_rrmc opcode rt rn AL in
  let ppi_rrrmc opcode rt ri rn c =
    pp_memoc opcode c^" "^pp_reg rt ^ ", "^
    "[" ^ pp_reg ri ^ "," ^ pp_reg rn ^ "]" in

  let ppi_rr opcode rt rn = opcode^" "^pp_reg rt ^ ", "^ pp_reg rn in
  let ppi_rrc opcode rt rn c=
    pp_memoc opcode c^" "^pp_reg rt ^ ", "^ pp_reg rn in
  let ppi_ri opcode r i = opcode^" "^pp_reg r ^ ", " ^ m.pp_k i in
  let ppi_ric opcode r i c=
    pp_memoc opcode c^" "^pp_reg r ^ ", " ^ m.pp_k i in

  fun i -> match i with
  | I_ADD(s,rt,rn,v) -> ppi_rri "ADD" s rt rn v
  | I_ADD3 (s,r1,r2,r3) -> ppi_rrr "ADD" s r1 r2 r3 
  | I_SUB(s,rt,rn,v) -> ppi_rri "SUB" s rt rn v
  | I_SUB3 (s,r1,r2,r3) -> ppi_rrr "SUB" s r1 r2 r3 
  | I_AND(s,rt,rn,v) -> ppi_rri "AND" s rt rn v
  | I_B v -> "B " ^ pp_lbl v
  | I_BEQ(v) -> "BEQ "^ pp_lbl v
  | I_BNE(v) -> "BNE "^ pp_lbl v
  | I_CMPI (r,v) -> ppi_ri "CMP" r v
  | I_CMP (r1,r2) -> ppi_rr "CMP" r1 r2
  | I_LDREX(rt,rn) -> ppi_rrm "LDREX" rt rn
  | I_LDR(rt,rn,c) -> ppi_rrmc "LDR" rt rn c
  | I_LDR3(rt,rn,rm,c) -> ppi_rrrmc "LDR" rt rn rm c
  | I_STR(rt,rn,c) -> ppi_rrmc "STR" rt rn c
  | I_STR3(rt,rn,rm,c) -> ppi_rrrmc "STR" rt rn rm c
  | I_MOVI(r,i,c) -> ppi_ric "MOV" r i c
  | I_MOV(r1,r2,c) -> ppi_rrc "MOV" r1 r2 c
  | I_XOR(s,r1,r2,r3) -> ppi_rrr "EOR" s r1 r2 r3
  | I_DMB  -> "DMB"
  | I_DSB  -> "DSB"
  | I_ISB  -> "ISB"
	

let pp_arm_instruction i =
  do_pp_instruction 
    {pp_k = pp_k_arm} i

(*
let dump_instruction =
  do_pp_instruction 
    {pp_k = (fun v -> "#" ^ string_of_int v)}
*)



let pp_instruction i = match i with
| PPC_ins i' -> pp_ppc_instruction i'
| ARM_ins i' -> pp_arm_instruction i'



(* pp of events ********************************************* *)

let pp_cst c =
  match c with
  | Concrete i -> string_of_int i
  | Symbolic s -> s

let pp_value v = 
  match v with
  | Rigid c -> pp_cst c
  | Flexible f -> sprintf "S%i" f



type pretty_event_id = 
  | PE_write of w_eiid
  | PE_barrier of b_eiid

let pretty_event_map = ref ([]:(pretty_event_id*string) list)

let pretty_ioid_map = ref ([]:(MachineDefFreshIds.ioid*string) list)


let pretty_event_names = ["a";"b";"c";"d";"e";"f";"g";"h";   "m";"n";"o";"p";"q";"r";"s";"t"]
let pretty_event_id_next = ref 0 
let pp_pretty_event_id n = if n < List.length pretty_event_names then List.nth pretty_event_names n else "t"^string_of_int n 
let next_pretty_event_id () = 
  let s = pp_pretty_event_id !pretty_event_id_next in
  pretty_event_id_next := 1 + !pretty_event_id_next;
  s


let initialise_pretty_event_map init_write_event_ids ss = 

  let pretty_initial_write_names = ["i";"j";"k";"l";] in

  let pp_pretty_initial_write n = if n < List.length pretty_initial_write_names then List.nth pretty_initial_write_names n else "i"^string_of_int n in

  let prettied_initial_writes : (pretty_event_id * string) list = 
    let rec f ws n =
      match ws with
      | [] -> []
      | w::ws' -> (PE_write w,pp_pretty_initial_write n) :: f ws' (n+1) in
    f init_write_event_ids 0 in

  pretty_event_map := prettied_initial_writes;


  let all_instructions_of_ts ts = List.rev (Pset.elements (Pset.union ts.committed_instructions ts.in_flight_instructions)) in

  let all_instructions_of_ss = 
    List.flatten (List.map 
      (fun t->
        all_instructions_of_ts (ss.thread_states t)
      )
      (List.rev (Pset.elements ss.storage_subsystem.threads))) in

  let prettiable_ioids_of_ss = 
    option_map  
      (fun i ->
        if 
          i.is_load || i.is_load_reserve || i.is_load_acquire || 
          i.is_store || i.is_store_conditional || i.is_store_release || 
          i.is_sync || i.is_lwsync || i.is_eieio
        then
          Some i.ioid 
        else 
          None)
      all_instructions_of_ss in


  let prettied_ioids : (MachineDefFreshIds.ioid * string) list = 
    let rec f ioids =
      match ioids with
      | [] -> []
      | ioid::ioids' -> (ioid,next_pretty_event_id ()) :: f ioids' in
    f prettiable_ioids_of_ss  in
 
  pretty_ioid_map := prettied_ioids





let pp_pretty_write_id  wid =
  try
    List.assoc (PE_write wid) (!pretty_event_map) 
  with 
    Not_found -> 
      try 
        List.assoc wid.weiid_ioid !pretty_ioid_map 
      with Not_found -> "??"


let pp_pretty_barrier_id  bid =
(*   try *)
(*     List.assoc (PE_barrier bid) (!pretty_event_map)  *)
(*   with  *)
(*     Not_found ->  *)
      try 
        List.assoc bid.beiid_ioid !pretty_ioid_map 
      with Not_found -> "??"

let pp_pretty_ioid ioid = 
  try 
    List.assoc ioid !pretty_ioid_map 
  with Not_found -> "??"




let pp_sync m = 
  match m.pp_arch with
  | PP_PPC -> "Sync"
  | PP_ARM -> "DMB"

let pp_barrier_type m b =
  match m.pp_arch with
  | PP_PPC -> 
      (match b with
      | Sync -> pp_sync m
      | LwSync -> "Lwsync"
      | Eieio -> "Eieio")
  | PP_ARM ->
      (match b with
      | Sync -> pp_sync m
      | LwSync -> assert(false)
      | Eieio -> assert(false))

        

let pp_write_uncoloured m w =
  sprintf "%s:W %s %s" (pp_pretty_write_id w.w_eiid) (*w.w_Thread*) (pp_value w.w_addr) (pp_value w.w_value)

let pp_barrier_uncoloured  m b =
  sprintf "%s:%s " (pp_pretty_barrier_id  b.b_eiid) (pp_barrier_type m b.b_barrier_type) 
(*  sprintf "(%d:%d):%s " b.b_thread b.b_eiid.beiid_ioid (pp_barrier_type m b.b_barrier_type) *)

let pp_read_response_uncoloured m rr =
  sprintf "%s:R %s %s" (pp_pretty_ioid rr.rr_eiid.reiid_ioid) (*w.w_Thread*) (pp_value rr.rr_write.w_addr) (pp_value rr.rr_write.w_value)


let pp_write_set_uncoloured m ws = String.concat "," (List.map (pp_write_uncoloured m) (Pset.elements ws))

let pp_read_response_set_uncoloured m rrs = String.concat "," (List.map (pp_read_response_uncoloured m) (Pset.elements rrs))


let pp_write m c w = 
  colour_old_new m c (pp_write_uncoloured m w)


let pp_write_option m c wo = 
  colour_old_new m c (match wo with None -> "clear" | Some w -> pp_write_uncoloured m w)

let pp_read_response m c rr = 
  colour_old_new m c (pp_read_response_uncoloured m rr)

let pp_barrier m c b =
  colour_old_new m c (pp_barrier_uncoloured m b) (*sprintf "%s %d %d" (pp_barrier_type b.b_barrier_type) b.b_thread b.b_eiid.beiid_ioid)*)






 
        



(* pp of transitions ************************************ *)

let pp_trans m t =
  match t with
  | Commit_write (tid,ii,ws) -> Printf.sprintf "(%d:%d) Commit write: %s: %s" tid  ii.ioid (pp_instruction ii.instruction) (colour_memory_action m (pp_write_set_uncoloured m ws))
(*  | Commit_write_release (tid,ii,ws) -> Printf.sprintf "(%d:%d) Commit write release: %s: %s" tid  ii.ioid (pp_instruction ii.instruction) (colour_memory_action m (pp_write_set_uncoloured m ws))*)
  | Commit_barrier (tid,ii,b) -> Printf.sprintf "(%d:%d) Commit barrier: %s: %s " tid  ii.ioid (pp_instruction ii.instruction) (colour_memory_action m (pp_barrier_uncoloured m b))
  | Commit_write_conditional (tid,ii,wcr,b) -> Printf.sprintf "(%d:%d) Commit write conditional: %s: [%s] %s (%s)" tid  ii.ioid (pp_instruction ii.instruction) (match wcr.wc_wprev with Some wprev -> colour_memory_action m (pp_write_uncoloured m wprev) | None -> "NONE") (colour_memory_action m (pp_write_uncoloured m wcr.wc_w)) (if b then "succeeding" else "failing")
  | Commit_read (tid,ii,rr) -> Printf.sprintf "(%d:%d) Commit read: %s: %s" tid  ii.ioid (pp_instruction ii.instruction) (colour_memory_action m (pp_read_response_set_uncoloured m rr))
  | Commit_reg_or_branch (tid,ii) -> Printf.sprintf "(%d:%d) Commit reg or branch: %s " tid  ii.ioid (pp_instruction ii.instruction)
  | Write_propagate_to_thread (w,tid) -> Printf.sprintf "(%d:) Write propagate to thread: %s to Thread %d " tid (colour_memory_action m (pp_write_uncoloured m w)) tid 
  | Barrier_propagate_to_thread (b,tid) -> Printf.sprintf "(%d:) Barrier propagate to thread: %s to Thread %d" tid (colour_memory_action m (pp_barrier_uncoloured m b)) tid
  | Read_from_storage_subsystem (tid,ii,w) -> Printf.sprintf "(%d:%d) Read from storage subsystem: %s (from %s) " tid ii.ioid (pp_instruction ii.instruction) (colour_memory_action m (pp_write_uncoloured m w))
(*  | Read_reserve_from_storage_subsystem (tid,ii,w) -> Printf.sprintf "(%d:%d) Read reserve from storage subsystem: %s (from %s) " tid ii.ioid (pp_instruction ii.instruction) (colour_memory_action m (pp_write_uncoloured m w))*)
(*  | Read_acquire_from_storage_subsystem (tid,ii,w) -> Printf.sprintf "(%d:%d) Read acquire from storage subsystem: %s (from %s) " tid ii.ioid (pp_instruction ii.instruction) (colour_memory_action m (pp_write_uncoloured m w))*)
  | Write_forward_to_read (tid,ii1,w,ii2) -> Printf.sprintf "(%d:%d<%d) Write forward to read: %s (%s from (%d:%d) %s) " tid ii1.ioid ii2.ioid (pp_instruction ii1.instruction) (colour_memory_action m (pp_write_uncoloured m w)) tid ii2.ioid (pp_instruction ii2.instruction)
  | Acknowledge_sync (b) -> Printf.sprintf "Acknowledge sync: %s %s" (pp_sync m) (colour_memory_action m (pp_barrier_uncoloured m b))
  | Write_reaching_coherence_point (w) -> Printf.sprintf "Write reaching coherence point: %s " (colour_memory_action m (pp_write_uncoloured m w))
  | Partial_coherence_commit (w1,w2) -> Printf.sprintf "Partial coherence commit: %s -> %s" (colour_memory_action m (pp_write_uncoloured m w1)) (colour_memory_action m (pp_write_uncoloured m w2))
  | Partial_evaluate (tid,ii) -> Printf.sprintf "(%d:%d) Partial evaluate: %s " tid ii.ioid (pp_instruction ii.instruction) 
  | Register_read_initial (tid,ii,r) -> Printf.sprintf "(%d:%d) Register read initial: %s (read %s) " tid ii.ioid (pp_instruction ii.instruction) (pp_reg r)
  | Register_read_prev (tid,ii1,r,ii2) -> Printf.sprintf "(%d:%d<%d) Register read prev: %s (read %s from %s) " tid ii1.ioid ii2.ioid (pp_instruction ii1.instruction) (pp_reg r) (pp_instruction ii2.instruction)
 

let pp_thread_trans_brief m t =
  match t with
  | Commit_write (tid,ii,ws) -> Printf.sprintf "Commit"
  | Commit_barrier (tid,ii,b) -> Printf.sprintf "Commit"
  | Commit_write_conditional (tid,ii,w,b) -> Printf.sprintf "Commit (%s)" (if b then "succ" else "fail")
  | Commit_read (tid,ii,rrs) -> Printf.sprintf "Commit"
  | Commit_reg_or_branch (tid,ii) -> Printf.sprintf "Commit"
  | Read_from_storage_subsystem (tid,ii,w) -> Printf.sprintf "Read %s" (pp_write_uncoloured m w)
(*  | Read_reserve_from_storage_subsystem (tid,ii,w) -> Printf.sprintf "Read reserve %s" (pp_write_uncoloured m w)*)
(* | Read_acquire_from_storage_subsystem (tid,ii,w) -> Printf.sprintf "Read acquire %s" (pp_write_uncoloured m w)*)
  | Write_forward_to_read (tid,ii1,w,ii2) -> Printf.sprintf "ReadFwd %s" (pp_write_uncoloured m w) 
  | Partial_evaluate (tid,ii) -> Printf.sprintf "Eval"
  | Register_read_initial (tid,ii,r) -> "Read reg initial"
  | Register_read_prev (tid,ii1,r,ii2) -> "Read reg prev"
  | _ -> raise (Failure "pp_thread_trans_brief called on non-thread transition")
 (* END ARG *)


(* relation auxiliaries ********************************* *)

    let rec option_map f xs = 
      match xs with 
      | [] -> [] 
      | x::xs -> 
          ( match f x with 
          | None -> option_map f xs 
          | Some x -> x :: (option_map f xs) ) 

    let rec reflexive_domain eq xs = 
      match xs with 
      | [] -> [] 
      | (x1,x2)::xs' -> 
          if eq x1 x2 then 
            x1::reflexive_domain eq xs'
          else
            reflexive_domain eq xs'

    let rec irreflexive_reduction eq xs = 
      match xs with 
      | [] -> [] 
      | (x1,x2)::xs' -> 
          if eq x1 x2 then 
            irreflexive_reduction eq xs' 
          else
            (x1,x2)::irreflexive_reduction eq xs'






(* **************************************************** *)
(* the main pp code                                     *)
(*                                                      *)
(* first we have code                                   *)  
(*                                                      *)
(*   make_ui_system_state_opt sopt ss' ncands           *)
(*                                                      *)
(* to build a Types.ui_system_state from an old system  *)
(* state, a new system state, and a list of numbered    *)
(* candidate transitions.                               *)
(*                                                      *)
(* Then we've have separate code,                       *)
(*                                                      *)
(*   pp_ui_system_state                                 *)
(*                                                      *)
(* to render that into ASCII or html.                   *)
(* **************************************************** *)



let pset_checked_diff s' s = 
  if Pset.subset s s' then 
    Pset.diff s' s 
  else
    raise (Failure "pset_checked_diff assert failure")

let rec check_list_prefix xs ys = 
  match xs,ys with
  | [],_ -> ()
  | (x::xs,y::ys) when x=y -> check_list_prefix xs ys
  | _ ->  raise (Failure "checked_list_prefix assert failure")

let rec checked_list_suffix xs ys = 
  match xs,ys with
  | [],ys -> ys
  | (x::xs,y::ys) when x=y -> checked_list_suffix xs ys
  | _ ->  raise (Failure "checked_list_suffix assert failure")


let is_ss_transition = 
  function 
    | Write_propagate_to_thread (_,_)
    | Barrier_propagate_to_thread (_,_)
    | Acknowledge_sync (_)
    | Write_reaching_coherence_point (_)
    | Partial_coherence_commit (_,_)
      -> true
    | _ 
      -> false


let is_instruction_instance_transition tid0 i0 = 
  (function 
    | Commit_write (tid,i,_) 
    | Commit_barrier (tid,i,_) 
    | Commit_write_conditional (tid,i,_,_) 
    | Commit_read (tid,i,_) 
    | Commit_reg_or_branch (tid,i) 
    | Read_from_storage_subsystem (tid,i,_) 
(*    | Read_reserve_from_storage_subsystem (tid,i,_)  *)
    | Write_forward_to_read (tid,i,_,_)  (* could be fancier - this is just attached to the reading instruction; it'd be nice to show the source *)
    | Partial_evaluate (tid,i) 
    | Register_read_prev (tid,i,_,_) (* again could be fancier *)
    | Register_read_initial (tid,i,_) 
      when tid=tid0 && i.ioid=i0.ioid
      -> true
    | _ 
      -> false)
          

let make_ui_storage_subsystem_state ss ss' ncands = {
  ui_threads = Pset.elements ss'.threads;
  ui_writes_seen_old = Pset.elements ss.writes_seen;
  ui_writes_seen_new = Pset.elements (pset_checked_diff ss'.writes_seen ss.writes_seen);
  ui_coherence_old = (transitive_reduction (fun w1 w2 -> w1=w2) 
                        (Pset.elements ss.coherence));
  ui_coherence_new = (transitive_reduction (fun w1 w2 -> w1=w2) 
                        (Pset.elements (pset_checked_diff ss'.coherence ss.coherence)));
  ui_writes_past_coherence_point_old = Pset.elements ss.writes_past_coherence_point;
  ui_writes_past_coherence_point_new = Pset.elements 
    (pset_checked_diff ss'.writes_past_coherence_point ss.writes_past_coherence_point);
  ui_events_propagated_to = (List.map 
                         (fun tid -> 
                           (tid,
                            (ss.events_propagated_to tid,
                             checked_list_suffix (ss.events_propagated_to tid) (ss'.events_propagated_to tid))))
                         (Pset.elements ss.threads));
  ui_unacknowledged_sync_requests_removed 
    = Pset.elements (Pset.diff ss.unacknowledged_sync_requests ss'.unacknowledged_sync_requests);
  ui_unacknowledged_sync_requests_old 
    = Pset.elements (Pset.inter ss.unacknowledged_sync_requests ss'.unacknowledged_sync_requests);
  ui_unacknowledged_sync_requests_new 
    = Pset.elements (Pset.diff ss'.unacknowledged_sync_requests ss.unacknowledged_sync_requests);
(*  ui_reservations = (List.map 
                         (fun tid -> 
                           (tid,
                            ss'.reservations tid,
                            (if ss'.reservations tid = ss.reservations tid then UI_old else UI_new)))
                         (Pset.elements ss.threads));     
*)
  ui_ss_transitions
    = List.filter (fun (n,t) -> is_ss_transition t) ncands;
} 


let make_ui_instruction_instance i i' kind = 
  assert (i.ioid=i'.ioid && i.instruction=i'.instruction && (*i.regs_in=i'.regs_in && i.regs_out=i'.regs_out &&*) i.program_loc=i'.program_loc && i.program_loc=i'.program_loc );
  {  
     ui_ioid = i.ioid;
     ui_kind = kind;
     ui_behaviour 
       = { (* ui_remaining_removed = List.rev (checked_list_suffix (List.rev i'.behaviour.remaining) (List.rev i.behaviour.remaining)); *)
           ui_remaining_now = i'.behaviour.remaining;
           ui_all_changed = (try let _ = List.rev (checked_list_suffix (List.rev i'.behaviour.remaining) (List.rev i.behaviour.remaining)) in false with Failure _ -> true);
           ui_val_soln_old = i.behaviour.val_soln;
           ui_val_soln_new = i'.behaviour.val_soln;
         } ;
    ui_regs_in = Pset.elements i.regs_in;
    ui_regs_out = Pset.elements i.regs_out;    
    ui_writes_read_from = 
     (match (i.writes_read_from, i'.writes_read_from, Pset.subset i.writes_read_from i'.writes_read_from) with
     | ws1,ws2,_ when Pset.is_empty ws1 && Pset.is_empty ws2 -> WRF_none
     | _,_,true -> WRF_some_new(Pset.elements i.writes_read_from, Pset.elements (pset_checked_diff i'.writes_read_from i.writes_read_from))
     | _,_,false -> WRF_all_new(Pset.elements i'.writes_read_from));
    ui_read_responses = 
     (match (i.read_responses, i'.read_responses, Pset.subset i.read_responses i'.read_responses) with
     | rrs1,rrs2,_ when Pset.is_empty rrs1 && Pset.is_empty rrs2 -> RR_none
     | _,_,true -> RR_some_new(Pset.elements i.read_responses, Pset.elements (pset_checked_diff i'.read_responses i.read_responses))
     | _,_,false -> RR_all_new(Pset.elements i'.read_responses));
    ui_program_loc = i.program_loc;     
    ui_instruction = i. instruction;
    ui_prev_old = i.prev;
    ui_prev_new = i'.prev; (* : (inst_occurrence_id * reaches_by) option *) (* CAN CHANGE *)
  }


let make_ui_thread_state tid ts ts' ncands = 
  assert (ts.thread=ts'.thread) (* && ts.initial_register_state=ts'.initial_register_state)*);
    {
     ui_thread = ts.thread ;                               
     ui_initial_register_state = [] (*: (reg * value) list*) (* TODO: here MachineDefTypes.ml has a function, but it'd be handier to have a map *) ;            
     ui_instructions =
     (* how to pick out the transitions relevant to an instruction *)     
     (let ui_trans_of i 
         = List.filter (fun (n,t) -> is_instruction_instance_transition tid i t) ncands in
     (* first get all instructions from ts' in a canonical order *)
     let all_new_instructions = Pset.elements (Pset.union ts'.committed_instructions ts'.in_flight_instructions) in
     (* and all instructions from ts *)
     let all_old_instructions = Pset.union ts.committed_instructions ts.in_flight_instructions in
     (* now work out what kind they are and annotate with any relevant transitions *)
     let all_ui_instructions = 
       List.map
         (fun i' ->
           let i'_committed = Pset.mem i' ts'.committed_instructions in
           let old_is = 
             Pset.elements 
               (Pset.filter 
                  (fun i -> i.ioid=i'.ioid) 
                  all_old_instructions) in
           match old_is with
           | [] -> 
               (match i'_committed with
               | true -> make_ui_instruction_instance i' i' UI_committed_new
               | false -> make_ui_instruction_instance i' i' (UI_in_flight (ui_trans_of i')))

           | [i] -> 
               (let i_committed = Pset.mem i ts.committed_instructions in
               match i_committed,i'_committed with
               | (true,true) -> make_ui_instruction_instance i i' UI_committed_old
               | (false,true) -> make_ui_instruction_instance i i' UI_committed_new
               | (false,false) -> make_ui_instruction_instance i i' (UI_in_flight (ui_trans_of i'))
               | (true,false) -> assert(false))
           | _ -> assert(false))
         all_new_instructions in
     all_ui_instructions);
    (*ui_writes_received : write list;*)
    ui_unacknowledged_syncs_removed = Pset.elements (Pset.diff ts.unacknowledged_syncs ts'.unacknowledged_syncs);
    ui_unacknowledged_syncs_old = Pset.elements (Pset.inter ts.unacknowledged_syncs ts'.unacknowledged_syncs);
    ui_unacknowledged_syncs_new = Pset.elements (Pset.diff ts'.unacknowledged_syncs ts.unacknowledged_syncs);
  }

let make_ui_system_state ss ss' ncands = {
  ui_model = ss'.model;
  ui_thread_states = List.map 
    (fun t->
       make_ui_thread_state t (ss.thread_states t) (ss'.thread_states t) ncands
    )
    (Pset.elements ss'.storage_subsystem.threads);
  ui_storage_subsystem = make_ui_storage_subsystem_state ss.storage_subsystem ss'.storage_subsystem ncands;
	  (* id_state : id_state;*)
   (* model : model_params *)
  }

let make_ui_system_state_opt sopt ss' ncands = 
  match sopt with
  | None -> make_ui_system_state ss' ss' ncands
  | Some ss -> make_ui_system_state ss ss' ncands


(*
let make_ui_system_state sopt s' ncands =
   raise (Failure "foo")
*)


(* **************************************************** *)
(* render into ASCII (or html, in due course)           *)
(* **************************************************** *)





let pp_tracked_event m c e =
  match e with
  | SWrite w -> pp_write m c w
  | SBarrier b -> pp_barrier m c b

let pp_coherence_edge m c (w1,w2) =
  colour_old_new m c (sprintf "%s -> %s" (pp_write_uncoloured m w1) (pp_write_uncoloured m w2))

let pp_list_sep m c = colour_old_new m c ", "

let pp_list m c pp_f l = String.concat (pp_list_sep m c) (List.map (pp_f m c) l)

let pp_list_core_old_new m pp_f l_old l_new = 
  pp_list m UI_old pp_f l_old 
  ^ (match l_old,l_new with (_::_,_::_) ->  pp_list_sep m UI_old  | _,_ -> "") 
  ^ pp_list m UI_new pp_f l_new

let pp_list_old_new m pp_f l_old l_new =
   "[" ^ pp_list_core_old_new m pp_f l_old l_new ^ "]"

let pp_leftbrace m =
  match m.Globals.pp_kind with
  | Ascii | Html -> "{"
  | Latex -> "\\mylb{}"

let pp_rightbrace m =
  match m.Globals.pp_kind with
  | Ascii | Html -> "}"
  | Latex -> "\\myrb{}"

let pp_set_old_new m pp_f l_old l_new =
  pp_leftbrace m ^ pp_list_core_old_new m pp_f l_old l_new ^ pp_rightbrace m

let pp_set_all_new m pp_f l_new =
  colour_old_new m UI_new (pp_leftbrace m) ^ pp_list_core_old_new m pp_f [] l_new ^ colour_old_new m UI_new (pp_rightbrace m)



let pp_cand m (n,c) =
  Printf.sprintf "%-2s    %s\n" (colour_tran_id m (sprintf "%-2d" n)) (colour_tran_id m (pp_trans m c));
  match m.Globals.pp_kind with
  | Html -> Printf.sprintf "  <spanid='%d'>%s</span>\n" n (colour_tran_id m (pp_trans m c))
  | _ -> Printf.sprintf "%-2s    %s\n" (colour_tran_id m (sprintf "%-2d" n)) (colour_tran_id m (pp_trans m c))

let pp_ui_storage_subsystem_state m model ss = 

  (* don't explicitly print:  ui_threads : thread_id list; *)

  let ppd_writes_seen = 
    pp_set_old_new m pp_write ss.ui_writes_seen_old ss.ui_writes_seen_new in

  let ppd_coherence = 
    pp_set_old_new m pp_coherence_edge ss.ui_coherence_old ss.ui_coherence_new in

  let pp_ss_transitions p = 
    String.concat "" 
      (opt_map 
         (function 
           | (n,c) when p (n,c) -> Some (pp_cand m (n,c))
           | _ -> None
         )
         ss.ui_ss_transitions) in

  let ppd_writes_past_coherence_point = 
    pp_set_old_new m pp_write ss.ui_writes_past_coherence_point_old ss.ui_writes_past_coherence_point_new  in

  let ppd_events_propagated_to =
    (String.concat "" 
       (List.map (fun (tid,(events_old,events_new)) -> 
         sprintf "    Thread %d: %s\n" tid
           (pp_list_old_new m pp_tracked_event events_old events_new))
          ss.ui_events_propagated_to)) in

  let ppd_unacknowledged_sync_requests =
    (* ignore for now: ui_unacknowledged_sync_requests_removed  *)
    pp_set_old_new m pp_barrier ss.ui_unacknowledged_sync_requests_old ss.ui_unacknowledged_sync_requests_new in

(*  let ppd_reservations =
    (String.concat "" 
       (List.map (fun (tid,wopt,is_new) -> 
         sprintf "    Thread %d: %s  " tid
           (pp_write_option m is_new wopt))
          ss.ui_reservations)) in
*)
  (* ignore for now:    ui_ss_transitions: ui_trans list;  *)
  	let res = 
  		match m.Globals.pp_kind with
  		| Html -> sprintf "<b>Storage subsystem state:</b><br/> <b> writes seen</b>  = %s <b> coherence  </b>  = %s  %s<b>events_propagated_to</b>:<br/>%s <b> unacknowledged_%s_requests</b> = %s "  (*<b> reservations </b> = %s*)
  		| _ -> sprintf "Storage subsystem state:\n  writes seen = %s  coherence = %s  %sevents_propagated_to:\n%s  unacknowledged_%s_requests = %s" (*  reservations = %s*)
	in
	
	 res  
        (ppd_writes_seen 
    ^ !linebreak)

    (ppd_coherence
    ^ !linebreak
    ^ pp_ss_transitions  
      (function   | (n,(Partial_coherence_commit (_,_))) -> true | _ -> false))

    (if model.ss.coherence_points then 
      "writes_past_coherence_point = "
      ^ ppd_writes_past_coherence_point
      ^ !linebreak
      ^ pp_ss_transitions  
          (function   | (n,(Write_reaching_coherence_point (_))) -> true | _ -> false)
      ^ "  "
    else
      "")

    (ppd_events_propagated_to         
    ^ pp_ss_transitions  
      (function   | (n,(Write_propagate_to_thread (_,_))) -> true | _ -> false)
    ^ pp_ss_transitions  
      (function   | (n,(Barrier_propagate_to_thread (_,_))) -> true | _ -> false))

    (pp_sync m)

    (ppd_unacknowledged_sync_requests
    ^ !linebreak
    ^ pp_ss_transitions  
      (function   | (n,(Acknowledge_sync (_))) -> true | _ -> false))

(*    (ppd_reservations
    ^ !linebreak
    )
*)


let pp_v_soln m soln v soln' v' = 
  let v_solved = subst_var soln v in
  let v'_solved = subst_var soln' v' in
  if v_solved = v'_solved then 
    pp_value v_solved
  else
    colour_old_new m UI_new (pp_value v'_solved)

let pp_action_raw a = match a with
  | Read_reg(r,v)->"RR"
  | Write_reg(r,v)->"WR"
  | Read_mem(a,v)->"RM"
  | Write_mem(a,v)->"WM"
  | Read_mem_reserve(a,v)->"RMR"
  | Write_mem_conditional(a,v1,v2)->"WMC"
  | Read_mem_acq(a,v)->"RMA"
  | Write_mem_rel(a,v)->"WMR"
  | Binop(v1,op,v2,v3)->"BINOP"
  | Unop(v1,op,v2)->"UNOP"
  | Barrier(b)->"BARR"
  | Isync->"ISYNC"
  | Jump(c)->"JMP"
  | Cond_branch(v,c)->"BR"


let pp_action m ioid soln a soln' a' = 
(*  Printf.printf "(%s,%s)  " (pp_action_raw a)(pp_action_raw a');flush stdout;*)
  let pp_ioid () = pp_pretty_ioid ioid ^ ":" in
  match a,a' with
(*       | Read_reg (r,v),Read_reg (r',v')  ->  *)
(*           assert(r=r'); *)
(* 	  ("r " ^ pp_reg r ^ "=" ^ (pp_v_soln m soln v soln' v')) *)
(*       | Write_reg (r,v),Write_reg (r',v') ->  *)
(*           assert(r=r'); *)
(* 	  ("w " ^ pp_reg r ^ "=" ^ (pp_v_soln m soln v soln' v')) *)
  | Read_reg (r,v),Read_reg (r',v')  -> 
      assert(r=r');
      ( (pp_v_soln m soln v soln' v') ^ "=" ^ pp_reg r)
  | Write_reg (r,v),Write_reg (r',v') -> 
      assert(r=r');
      (pp_reg r ^ "=" ^ (pp_v_soln m soln v soln' v'))
(* *)
  | Read_mem (a,v),Read_mem (a',v') -> 
      colour_memory_action m (pp_ioid ()^"R " ^ (pp_v_soln m soln a soln' a') ^ "=" ^ (pp_v_soln m soln v soln' v'))
  | Write_mem (a,v),Write_mem (a',v') -> 
      colour_memory_action m (pp_ioid ()^"W " ^ (pp_v_soln m soln a soln' a') ^ "=" ^ (pp_v_soln m soln v soln' v'))
  | Read_mem_reserve (a,v),Read_mem_reserve (a',v') -> 
      colour_memory_action m (pp_ioid ()^"RX " ^ (pp_v_soln m soln a soln' a') ^ "=" ^ (pp_v_soln m soln v soln' v'))
  | Write_mem_conditional (a,v1,v2),Write_mem_conditional (a',v1',v2') -> 
      colour_memory_action m (pp_ioid ()^"WX " ^ (pp_v_soln m soln a soln' a') ^ "=" ^ (pp_v_soln m soln v1 soln' v1')^" / "^ (pp_v_soln m soln v2 soln' v2'))
  | Binop (v1,op,v2,v3),Binop (v1',op',v2',v3') -> 
      assert(op=op');
      ((pp_v_soln m soln v1 soln' v1') ^ "=" ^ (pp_v_soln m soln v2 soln' v2') ^ pp_op op ^ (pp_v_soln m soln v3 soln' v3'))
  | Unop (v1,op,v2),Unop (v1',op',v2') -> 
      assert(op=op');
      ((pp_v_soln m soln v1 soln' v1') ^ "=" ^ pp_op1 op ^ (pp_v_soln m soln v2 soln' v2'))
  | Barrier b, Barrier b' ->
      assert(b=b');
      colour_memory_action m (pp_ioid ()^pp_barrier_type m b)
  | Isync,Isync -> colour_memory_action m "Isync"
  | Jump l,Jump l' ->
      assert(l=l');
      ("Jump: " ^ pp_cst l)
  | Cond_branch (v,l),Cond_branch (v',l') -> 
      assert(l=l');
      ("Branch on " ^ (pp_v_soln m soln v soln' v') ^ " to " ^ pp_cst l)
  | _,_ -> raise (Failure "mismatch in pp_action")


let pp_sem_state  m ioid s = 
  (* ignore for now: ui_remaining_removed : action list; *)
  (* TODO: suppress final register writes if they will already have been propagated *)
  match s.ui_all_changed with
  | false -> 
      "[" ^ String.concat "; " (List.map (fun a -> pp_action m ioid s.ui_val_soln_old a s.ui_val_soln_new a) s.ui_remaining_now) ^ "]"
  | true -> 
      colour_old_new m UI_new ("[" ^ String.concat "; " (List.map (fun a -> pp_action m ioid s.ui_val_soln_new a s.ui_val_soln_new a) s.ui_remaining_now) ^ "]")



(* TODO: should carry the proper substitution through to here - eg 
try ppoa-r... *)

let pp_reaches m rold rnew soln soln' = 
  match (rold,rnew) with
  | Always,Always -> ""
  | IfZero v1,IfZero v2 -> " if "^(pp_v_soln m soln v1 soln' v2) ^ "==0"
  | IfNonZero v1,IfNonZero v2 -> " if "^(pp_v_soln m soln v1 soln' v2) ^ "<>0"
  | _,_ -> raise (Failure "mismatch in pp_reaches")
                                           
let pp_prev0 m ipold ipnew soln soln' = 
  match (ipold,ipnew) with 
  | None,None -> "-" 
  | Some (p1,r1),Some(p2,r2) -> 
      assert(p1 = p2);
      string_of_int p1 ^ pp_reaches m r1 r2 soln soln'


let pp_ui_instruction_instance m tid i = 

  let ppd_trans_ids = 
    let trans_ids = 
      match i.ui_kind with
      | UI_in_flight ncands -> 
          ncands (* (List.map (fun (n,tr) -> n) ncands)*)
      | _ -> [] in
  colour_tran_id m 
      (sprintf "%-14s" 
         (String.concat " and " 
            (List.map 
               (function (n,tr) -> (
			   	let res = 
					match ((Globals.get_our_runopts()).Globals.ppkind) with
					| Html -> sprintf "<spanid='%s'>%s</span>"
					| _ -> sprintf "%s %-11s"
				in 
				res
                   (string_of_int n)
                   (pp_thread_trans_brief m tr)))
               trans_ids))) in

  let ppd_delim_l,ppd_delim_r = match i.ui_kind with
  | UI_committed_old -> "[|", "|]"
  | UI_committed_new -> colour_old_new m UI_new "[|", colour_old_new m UI_new "|]"
  | UI_in_flight _ ->   "<|", "|>" in

  let ppd_instruction = sprintf "(%d:%d) : %-9s" tid i.ui_ioid (pp_instruction i.ui_instruction) in

  let ppd_prev = pp_prev0 m i.ui_prev_old i.ui_prev_new i.ui_behaviour.ui_val_soln_old i.ui_behaviour.ui_val_soln_new in 

  let ppd_behaviour = pp_sem_state m i.ui_ioid i.ui_behaviour in

  let ppd_read_responses = 
    match i.ui_read_responses with
    | RR_none -> ""
    | RR_some_new(old,nw) ->  
        sprintf "; reads=%s" 
          (pp_set_old_new m pp_read_response old nw) 
    | RR_all_new(nw) ->  
        sprintf "; reads=%s" 
          (pp_set_all_new m pp_read_response nw) in

  let ppd_writes_read_from = 
    match i.ui_writes_read_from with
    | WRF_none -> ""
    | WRF_some_new(old,nw) ->  
        sprintf "; writes_read_from=%s" 
          (pp_set_old_new m pp_write old nw) 
    | WRF_all_new(nw) ->  
        sprintf "; writes_read_from=%s" 
          (pp_set_all_new m pp_write nw) 
  in

  let res = 
  	match ((Globals.get_our_runopts()).Globals.ppkind) with 
	| Html -> sprintf "<tr><td>%-14s</td><td> %s</td><td> %s; </td><td>prev = %2s; </td><td>behaviour = %s%s </td><td>%s</td></tr>"
	| _ -> sprintf "%-14s %s %-23s; prev = %2s; behaviour = %-11s%s %s\n"
	in 
	res
    ppd_trans_ids
    ppd_delim_l
    ppd_instruction
    ppd_prev
    ppd_behaviour
    (ppd_read_responses ^ ppd_writes_read_from)
    ppd_delim_r


    (* ignore for now:   ui_ioid : inst_occurrence_id;  *)
    (* ignore for now:   ui_regs_in : reg list;         *)
    (* ignore for now:   ui_regs_out : reg list;        *)
    (* ignore for now:   ui_program_loc : address;      *)



let pp_ui_thread_state m ts = 

  let ppd_tid = sprintf "%d" ts.ui_thread in

  let ppd_instructions = 
    String.concat "" (List.map (pp_ui_instruction_instance m ts.ui_thread) ts.ui_instructions) in

  let ppd_unacknowledged_syncs = 
    pp_set_old_new m pp_barrier ts.ui_unacknowledged_syncs_old ts.ui_unacknowledged_syncs_new in

  (* ignore for now:  ui_initial_register_state : (reg * value) list ;            *)
  (* ignore for now:  ui_unacknowledged_syncs_removed : barrier list ; *)

   let t_state = ( match ((Globals.get_our_runopts()).Globals.ppkind) with 
   					| Html -> sprintf "<b>Thread %s state:</b>" ppd_tid 
				 	| _ -> sprintf "Thread %s state:" ppd_tid
				) in

   let t_instructions = ( match ((Globals.get_our_runopts()).Globals.ppkind) with 
   							| Html -> sprintf "<table>%s</table>" ppd_instructions
				 			| _ -> sprintf "  committed [| |] and in-flight <| |> instructions:\n%s" ppd_instructions
						) in

   let t_sync = sprintf "  unacknowledged_%ss = %s " (pp_sync m) ppd_unacknowledged_syncs in
	t_state ^ !linebreak ^ t_instructions ^ t_sync ^ !linebreak
(*  sprintf 
    "Thread %s state:\n  committed [| |] and in-flight <| |> instructions:\n%s  unacknowledged_syncs = %s\n" 
    ppd_tid
    ppd_instructions
    ppd_unacknowledged_syncs*)



let pp_ui_system_state m s  = 
  pp_ui_storage_subsystem_state m s.ui_model s.ui_storage_subsystem
  ^ !linebreak
  ^ String.concat !linebreak (List.map (pp_ui_thread_state m) s.ui_thread_states)

