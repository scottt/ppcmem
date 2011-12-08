(*********************************************************************)
(*                        Memevents                                  *)
(*                                                                   *)
(* Jade Alglave, Luc Maranget, INRIA Paris-Rocquencourt, France.     *)
(* Susmit Sarkar, Peter Sewell, University of Cambridge, UK.         *)
(*                                                                   *)
(*  Copyright 2010 Institut National de Recherche en Informatique et *)
(*  en Automatique and the authors. All rights reserved.             *)
(*  This file is distributed  under the terms of the Lesser GNU      *)
(*  General Public License.                                          *)
(*********************************************************************)

open Printf
open Misc

(* Who am i ? *)
let arch = Misc.ARM

(*************)
(* Registers *)
(*************)

type reg = 
  | R0 | R1 | R2 | R3
  | R4 | R5 | R6 | R7
  | R8 | R9 | R10 | R11
  | R12 
  | SP | LR | PC
      
  | Z  (* condition flags *)
      
  | Symbolic_reg of string
  | Internal of int


let base =  Internal 0
and max_idx = Internal 1
and idx = Internal 2
and ephemeral = Internal 3
let loop_idx = Internal 4

let pc = PC	  

let regs =
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
   PC, "PC" ;
   Z, "Z" ;
 ]
    
let parse_list = List.map (fun (r,s) -> s,r) regs
    
let parse_reg s =
  try Some (List.assoc s parse_list)
  with Not_found -> None
      
let pp_reg r = match r with 
| Symbolic_reg r -> "%"^r
| Internal i -> Printf.sprintf "i%i" i
| _ -> try List.assoc r regs with Not_found -> assert false
	  

let reg_compare = Pervasives.compare 


(************)
(* Barriers *)
(************)

type barrier = 
  | DMB
  | DSB
  | ISB

let all_kinds_of_barriers = [DMB; ]

let pp_barrier b = match b with
| DMB -> "DMB"
| DSB -> "DSB"
| ISB -> "ISB"

let barrier_compare = Pervasives.compare

(****************)
(* Instructions *)
(****************)

type k = int 

type lbl = string

type setflags = SetFlags | DontSetFlags

type condition = NE | EQ | AL (* ALWAYS *)

type instruction =
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
  | I_LDR3 of reg * reg * reg * condition
  | I_STR of reg * reg * condition
  | I_STR3 of reg * reg * reg * condition
  | I_STREX of reg * reg * reg * condition
  | I_MOVI of reg * k * condition
  | I_MOV of reg * reg * condition
  | I_XOR of setflags * reg * reg * reg
  | I_DMB
  | I_DSB
  | I_ISB
	    

open SymbConstant

let pp_abs = function
  | Symbolic s -> s
  | Concrete i -> string_of_int i

let pp_lbl = fun i -> i

let pp_hash m = match m with
| Ascii | Dot -> "#"
| Latex -> "\\#"
| DotFig -> "\\\\#"

let pp_k m v = pp_hash m ^ string_of_int v

type basic_pp = { pp_k : k -> string; }

let pp_memo memo = function
  | SetFlags -> memo ^ "S"
  | DontSetFlags -> memo

let pp_condition = function
  | NE -> "NE"
  | EQ -> "EQ"
  | AL -> ""

let pp_memoc memo c = sprintf "%s%s" memo (pp_condition c)

let rec do_pp_instruction m = 
  let ppi_rrr opcode s rt rn rm =
    pp_memo opcode s^" "^ pp_reg rt ^ "," ^ pp_reg rn ^ "," ^ pp_reg rm in
  let ppi_rri opcode s rt rn v =
     pp_memo opcode s^" "^pp_reg rt ^ ","^ pp_reg rn ^ "," ^ m.pp_k v in
  let _ppi_rrv opcode rt rn v =
    opcode^" "^pp_reg rt ^ ","^ pp_reg rn ^ "," ^ pp_abs v in
  let ppi_rrmc opcode rt rn c=
    pp_memoc opcode c^" "^pp_reg rt ^
    ","^ "[" ^ pp_reg rn ^ "]" in
  let ppi_rrm opcode rt rn =  ppi_rrmc opcode rt rn AL in

  let ppi_rrrmc opcode rt ri rn c =
    pp_memoc opcode c^" "^pp_reg rt ^ ","^
    "[" ^ pp_reg ri ^ "," ^ pp_reg rn ^ "]" in

  let ppi_strex opcode rt rn rm c =
     pp_memoc opcode c^" "^pp_reg rt ^ ","^
    pp_reg rn ^ ",[" ^ pp_reg rm ^ "]" in

  let ppi_rr opcode rt rn = opcode^" "^pp_reg rt ^ ","^ pp_reg rn in
  let ppi_rrc opcode rt rn c=
    pp_memoc opcode c^" "^pp_reg rt ^ ","^ pp_reg rn in
  let ppi_ri opcode r i = opcode^" "^pp_reg r ^ "," ^ m.pp_k i in
  let ppi_ric opcode r i c=
    pp_memoc opcode c^" "^pp_reg r ^ "," ^ m.pp_k i in

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
  | I_STREX(rt,rn,rm,c) -> ppi_strex "STREX" rt rn rm c
  | I_MOVI(r,i,c) -> ppi_ric "MOV" r i c
  | I_MOV(r1,r2,c) -> ppi_rrc "MOV" r1 r2 c
  | I_XOR(s,r1,r2,r3) -> ppi_rrr "EOR" s r1 r2 r3
  | I_DMB  -> "DMB"
  | I_DSB  -> "DSB"
  | I_ISB  -> "ISB"
	

let pp_instruction m =
  do_pp_instruction 
    {pp_k = pp_k m}

let dump_instruction =
  do_pp_instruction 
    {pp_k = (fun v -> "#" ^ string_of_int v)}

(****************************)
(* Symbolic registers stuff *)
(****************************)

let allowed_for_symb =
  [ R0 ; R1 ; R2 ; R3 ; R4 ; R5 ; R6 ; 
    R7 ; R8 ; R9 ; R10; R11; R12 ]

let rec fold_regs (f_reg,f_sreg) =

  let fold_reg reg (y_reg,y_sreg) = match reg with
  | R0 | R1 | R2 | R3 | R4 | R5 | R6 | R7 | R8 
  | R9 | R10 | R11 | R12 | SP | LR | PC | Z ->  f_reg reg y_reg,y_sreg
  | Symbolic_reg reg -> y_reg,f_sreg reg y_sreg
  | Internal _ -> y_reg,y_sreg in

  fun c ins -> match ins with
  | I_ADD (_,r1, r2, _) 
  | I_SUB (_,r1, r2, _) 
  | I_AND (_,r1, r2, _)
  | I_LDR (r1, r2, _) 
  | I_LDREX (r1, r2) 
  | I_STR (r1, r2, _)
  | I_MOV (r1, r2, _)
  | I_CMP (r1,r2)
      -> fold_reg r2 (fold_reg r1 c)
  | I_LDR3 (r1, r2, r3, _)
  | I_ADD3 (_, r1, r2, r3) 
  | I_SUB3 (_, r1, r2, r3) 
  | I_STR3 (r1, r2, r3, _)
  | I_STREX (r1, r2, r3, _)
  | I_XOR (_,r1, r2, r3)
      -> fold_reg r3 (fold_reg r2 (fold_reg r1 c))
  | I_CMPI (r, v)
  | I_MOVI (r, v, _)
      -> fold_reg r c
  | I_B _
  | I_BEQ _
  | I_BNE _
  | I_DMB
  | I_DSB
  | I_ISB
      -> c


let rec map_regs f_reg f_symb =

  let map_reg  reg = match reg with
  | R0 | R1 | R2 | R3 | R4 | R5 | R6 | R7 | R8 
  | R9 | R10 | R11 | R12 | SP | LR | PC | Z -> f_reg reg 
  | Symbolic_reg reg -> f_symb reg
  | Internal _ -> reg in

  fun ins -> match ins with
  | I_ADD (s,r1, r2, k) -> I_ADD (s,map_reg r1, map_reg r2, k)
  | I_ADD3 (s,r1, r2, r3) -> I_ADD3 (s,map_reg r1, map_reg r2, map_reg r3)
  | I_SUB (s,r1, r2, k) -> I_SUB (s,map_reg r1, map_reg r2, k)
  | I_SUB3 (s,r1, r2, r3) -> I_SUB3 (s,map_reg r1, map_reg r2, map_reg r3)
  | I_AND (s,r1, r2, k) -> I_AND (s,map_reg r1, map_reg r2, k)
  | I_B _
  | I_BEQ _ 
  | I_BNE _ -> ins
  | I_CMPI (r, k) -> I_CMPI (map_reg r, k)
  | I_CMP (r1, r2) -> I_CMP (map_reg r1, map_reg r2)
  | I_LDREX (r1, r2) -> I_LDREX (map_reg r1, map_reg r2)
  | I_LDR (r1, r2, c) -> I_LDR (map_reg r1, map_reg r2, c)
  | I_LDR3 (r1, r2, r3, c) -> I_LDR3 (map_reg r1, map_reg r2, map_reg r3, c)
  | I_STR (r1, r2, c) -> I_STR (map_reg r1, map_reg r2, c)
  | I_STR3 (r1, r2, r3, c) -> I_STR3 (map_reg r1, map_reg r2, map_reg r3, c)
  | I_STREX (r1, r2, r3, c) -> I_STREX (map_reg r1, map_reg r2, map_reg r3, c)
  | I_MOVI (r, k, c) -> I_MOVI (map_reg r, k, c)
  | I_MOV (r1, r2, c) -> I_MOV (map_reg r1, map_reg r2, c)
  | I_XOR (s,r1, r2, r3) -> I_XOR (s,map_reg r1, map_reg r2, map_reg r3)
  | I_DMB -> I_DMB
  | I_DSB -> I_DSB
  | I_ISB -> I_ISB

(* No addresses burried in ARM code *)
let fold_addrs f c ins = c

let map_addrs f ins = ins

let norm_ins ins = ins

(* PLDI submission, complete later *)
let is_data _ _ = assert false

include Pseudo.Make
    (struct
      type ins = instruction
      type reg_arg = reg

      let fold_labels k f = function
        | I_B lbl
        | I_BEQ lbl
        | I_BNE lbl
            -> f k lbl
        | _ -> k

      let map_labels f = function
        | I_B lbl -> I_B (f lbl)
        | I_BEQ lbl -> I_BEQ (f lbl)
        | I_BNE lbl -> I_BNE (f lbl)
        | ins -> ins

    end)

let get_macro name = raise Not_found
