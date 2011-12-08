open MachineDefUtils
open MachineDefValue
open MachineDefTypes
open Types

(* ***************************************************************** *)
(* comparisons of various model types                                *)
(* ***************************************************************** *)


let int_of_comparison c = 
  match c with
  | Equal -> 0
  | Less -> -1
  | Greater -> 1

let write_compare w1 w2 = Pervasives.compare w1.w_eiid w2.w_eiid
let barrier_compare b1 b2 = Pervasives.compare b1.b_eiid b2.b_eiid

let tracked_event_compare t1 t2 = match t1,t2 with
| SWrite _,SBarrier _ -> -1
| SBarrier _,SWrite _ -> 1
| SWrite w1,SWrite w2 -> write_compare w1 w2
| SBarrier b1,SBarrier b2 -> barrier_compare b1 b2
      
let rec list_compare cmp xs ys = match xs,ys with
| [],[] -> 0
| [],_::_ -> -1
| _::_,[] -> 1
| x::xs,y::ys ->
    begin match cmp x y with
    | 0 -> list_compare cmp xs ys
    | c -> c
    end

exception Different of int

let fun_compare dom cmp f1 f2 =
  try
    Pset.iter
      (fun x -> match cmp (f1 x) (f2 x) with
      | 0 -> ()
      | c -> raise (Different c))
      dom ;
    0
  with Different c -> c

      

let storage_compare st1 st2 =
  let c = Pset.compare st1.threads st2.threads in
  if c <> 0 then c
  else
    let c = Pset.compare st1.coherence st2.coherence in
    if c <> 0 then c
    else
      let c = Pset.compare
          st1.writes_past_coherence_point
          st2.writes_past_coherence_point in
      if c <> 0 then c
      else
        let c =
          fun_compare st1.threads
            (list_compare tracked_event_compare)
            st1.events_propagated_to st2.events_propagated_to in
        if c <> 0 then c
        else
          Pset.compare
            st1.unacknowledged_sync_requests
            st2.unacknowledged_sync_requests

let compare_instruction_instance i1 i2 = Pervasives.compare i1.ioid i2.ioid

let subst_action sol act = 
  match act with
  | Read_reg (r,v) -> Read_reg (r,subst_var sol v)
  | Write_reg (r,v) -> Write_reg (r,subst_var sol v)
  | Read_mem (a,v) -> Read_mem (subst_var sol a,subst_var sol v)
  | Write_mem (a,v) -> Write_mem (subst_var sol a,subst_var sol v)
  | Read_mem_reserve (a,v) -> Read_mem_reserve (subst_var sol a,subst_var sol v)
  | Write_mem_conditional (a,v1,v2) -> Write_mem_conditional (subst_var sol a,subst_var sol v1,subst_var sol v2) 
  | Binop (v1,op,v2,v3) -> Binop (subst_var sol v1,op,subst_var sol v2,subst_var sol v3)
  | Unop (v1,op,v2) -> Unop (subst_var sol v1,op,subst_var sol v2)
  | Barrier (b) -> Barrier (b)
  | Isync -> Isync
  | Jump (c) -> Jump (c)
  | Cond_branch (v,c) -> Cond_branch (subst_var sol v,c)

let rec subst_actions sol acts = 
  match acts with
  | [] -> []
  | act :: acts -> subst_action sol act :: subst_actions sol acts

let semantics_tm_compare s1 s2 =
  let a1s = subst_actions s1.val_soln s1.remaining in
  let a2s = subst_actions s2.val_soln s2.remaining in
  Pervasives.compare a1s a2s

let inst_compare i1 i2 = 
  let c = Pervasives.compare i1.ioid i2.ioid in
  if c != 0 then c else
  let c = semantics_tm_compare i1.behaviour i2.behaviour in
  if c != 0 then c else
  let c = Pset.compare i1.regs_in i2.regs_in in
  if c != 0 then c else
  let c = Pset.compare i1.regs_out i2.regs_out in
  if c != 0 then c else
  let c = Pset.compare i1.writes_read_from i2.writes_read_from in
  if c != 0 then c else
  let c = int_of_comparison (compare_value i1.program_loc i2.program_loc) in
  if c != 0 then c else
  let c = Pervasives.compare i1.instruction i2.instruction in
  if c != 0 then c else
  Pervasives.compare i1.prev i2.prev

let rec inst_list_compare ist1 ist2 =
  match ist1,ist2 with
  | [],[] -> 0
  | i :: _,[] -> 1
  | [],i :: _ -> -1
  | i1 :: ist1', i2 :: ist2' ->
      let c = inst_compare i1 i2 in 
      if c != 0 then c else
      inst_list_compare ist1' ist2' 

let instruction_set_compare is1 is2 =
  let ist1 = Pset.elements is1 in
  let ist2 = Pset.elements is2 in
  inst_list_compare ist1 ist2

let thread_state_compare th1 th2 =
  let c = Misc.int_compare th1.thread th2.thread in
  if c <> 0 then c
  else (* Ignore irv field that are the same for a given thread *)
    let c = instruction_set_compare
        th1.in_flight_instructions
        th2.in_flight_instructions in
    if c <> 0 then c
    else
      let c = instruction_set_compare
          th1.committed_instructions
          th2.committed_instructions in
      if c <> 0 then c
      else
        Pset.compare
          th1.unacknowledged_syncs
          th2.unacknowledged_syncs

let system_state_compare st1 st2 =
  let c = storage_compare st1.storage_subsystem st2.storage_subsystem in
  if c <> 0 then c
  else
    let tids = st1.storage_subsystem.threads in
    fun_compare tids
      thread_state_compare st1.thread_states st2.thread_states


(* dead code? 
      type interactive_trans_data =
          { (* for Commit_instruction, Read_from_storage_subsystem, and Write_forward_to_read *)
            per_instruction : ((thread_id * inst_occurrence_id) * int) list;
            (* for Write_announce_to_thread *)
            per_write_ann : (write * int) list;
            (* for Write_reaching_coherence_point *)
            per_write_coh : (write * int) list;
            (* for Barrier_propagate_to_thread *)
            per_barrier_prop : (barrier * int) list;
            (* for Acknowledge_sync *)
            per_barrier_ack : (barrier * int) list; 
            (* for Partial Coherence commit *)
            per_partial_coh_commit : ((write * write) * int) list
          }

*)



(* ***************************************************************** *)
(* comparison of transitions, for user-interface sorting and for set implementation *)
(* ***************************************************************** *)


let compare_trans t1 t2 =
  let trans_order t = 
    match t with
    | Partial_coherence_commit _ ->             (0,0,0)
    | Write_reaching_coherence_point _ ->       (1,0,0)
    | Write_propagate_to_thread _ ->             (2,0,0)
    | Barrier_propagate_to_thread _ ->          (3,0,0)
    | Acknowledge_sync _ ->                     (4,0,0)
    | Commit_write(tid,ii,ws) ->             (5,tid,ii.ioid)
    | Commit_barrier(tid,ii,b) ->             (6,tid,ii.ioid)
    | Commit_write_conditional(tid,ii,w,b) ->             (7,tid,ii.ioid)
(*    | Commit_write_release(tid,ii,ws) ->      (8,tid,ii.ioid)*)
    | Commit_read(tid,ii,rrs) ->             (9,tid,ii.ioid)
    | Commit_reg_or_branch(tid,ii) ->             (10,tid,ii.ioid)
    | Read_from_storage_subsystem (tid,ii,_) -> (11,tid,ii.ioid)
(*    | Read_reserve_from_storage_subsystem (tid,ii,_) -> (12,tid,ii.ioid)*)
    | Write_forward_to_read (tid,ii1,_,ii2) ->  (13,tid,ii2.ioid)
    | Partial_evaluate (tid,ii) ->              (14,tid,ii.ioid)
    | Register_read_prev (tid,ii1,r,ii2) ->     (15,tid,ii2.ioid)
    | Register_read_initial  (tid,ii,_) ->     (16,tid,ii.ioid)
  in
  compare (trans_order t1) (trans_order t2)

let trans_compare t1 t2 =
  let tcount t = 
    match t with
    | Commit_write _ -> 0
    | Commit_barrier _ -> 1
    | Commit_write_conditional _ -> 2
(*    | Commit_write_release _ -> 3*)
    | Commit_read _ -> 4
    | Commit_reg_or_branch _ -> 5
    | Write_propagate_to_thread _ -> 6
    | Barrier_propagate_to_thread _ -> 7
    | Read_from_storage_subsystem _ -> 8
(*    | Read_reserve_from_storage_subsystem _ -> 9*)
(*    | Read_acquire_from_storage_subsystem _ -> 10*)
    | Write_forward_to_read _ -> 11
    | Acknowledge_sync _ -> 12
    | Partial_coherence_commit _ -> 13
    | Partial_evaluate _ -> 14
    | Register_read_prev _ -> 15
    | Register_read_initial _ -> 16
    | Write_reaching_coherence_point _ -> 17
  in
  let t1c = tcount t1 in
  let t2c = tcount t2 in
  let eqp = Pervasives.compare t1c t2c in
  let eqpair f g (x1,y1) (x2,y2) = let cx = f x1 x2 in if cx <> 0 then cx else g y1 y2 in
  if eqp = 0 then
    match (t1,t2) with
    | Write_propagate_to_thread (e1,t1), Write_propagate_to_thread (e2,t2)
        -> eqpair Pervasives.compare Pervasives.compare  (e1,t1) (e2,t2)
    | Barrier_propagate_to_thread (e1,t1), Barrier_propagate_to_thread (e2,t2)
        -> eqpair Pervasives.compare Pervasives.compare  (e1,t1) (e2,t2)
    | Acknowledge_sync e1,Acknowledge_sync e2
      -> Pervasives.compare e1 e2
    | Partial_coherence_commit (w11,w12),Partial_coherence_commit (w21,w22)
      -> Pervasives.compare (w11,w12) (w21,w22)
    | Write_reaching_coherence_point e1,Write_reaching_coherence_point e2
      -> Pervasives.compare e1 e2
    | Partial_evaluate (t1,i1), Partial_evaluate (t2,i2) ->
        if t1 = t2 then compare_instruction_instance i1 i2 else Pervasives.compare t1 t2
    | Commit_write (t1,i1,ws1),Commit_write (t2,i2,ws2) -> 
        if t1 = t2 then compare_instruction_instance i1 i2 else Pervasives.compare t1 t2
     | Commit_barrier (t1,i1,b1),Commit_barrier (t2,i2,b2) -> 
        if t1 = t2 then compare_instruction_instance i1 i2 else Pervasives.compare t1 t2
     | Commit_write_conditional (t1,i1,w1,b1),Commit_write_conditional (t2,i2,w2,b2) -> 
        if t1 = t2 then eqpair compare_instruction_instance Pervasives.compare (i1,b1) (i2,b2) else Pervasives.compare t1 t2
(*     | Commit_write_release (t1,i1,ws1),Commit_write_release (t2,i2,ws2) -> 
        if t1 = t2 then compare_instruction_instance i1 i2 else Pervasives.compare t1 t2*)
     | Commit_read (t1,i1,rr1),Commit_read (t2,i2,rr2) -> 
        if t1 = t2 then compare_instruction_instance i1 i2 else Pervasives.compare t1 t2
     | Commit_reg_or_branch (t1,i1),Commit_reg_or_branch (t2,i2) -> 
        if t1 = t2 then compare_instruction_instance i1 i2 else Pervasives.compare t1 t2
     | Read_from_storage_subsystem (t1,i1,w1),Read_from_storage_subsystem (t2,i2,w2) ->
         if (t1,w1) = (t2,w2) then compare_instruction_instance i1 i2 else Pervasives.compare (t1,w1) (t2,w2)
(*     | Read_reserve_from_storage_subsystem (t1,i1,w1),Read_reserve_from_storage_subsystem (t2,i2,w2) ->
         if (t1,w1) = (t2,w2) then compare_instruction_instance i1 i2 else Pervasives.compare (t1,w1) (t2,w2)*)
(*     | Read_acquire_from_storage_subsystem (t1,i1,w1),Read_acquire_from_storage_subsystem (t2,i2,w2) ->
         if (t1,w1) = (t2,w2) then compare_instruction_instance i1 i2 else Pervasives.compare (t1,w1) (t2,w2)*)
     | Register_read_initial (t1,i1,r1), Register_read_initial (t2,i2,r2) ->
         if (t1,r1) = (t2,r2) then compare_instruction_instance i1 i2 else Pervasives.compare (t1,r1) (t2,r2)
     | Write_forward_to_read (t1,ii1,e1,io1), Write_forward_to_read (t2,ii2,e2,io2) ->
        if (t1,e1) = (t2,e2) 
        then
          Pervasives.compare (ii1.ioid,io1.ioid) (ii2.ioid,io2.ioid)
        else
          Pervasives.compare (t1,e1) (t2,e2)
    | Register_read_prev (t1,ii1,e1,io1), Register_read_prev (t2,ii2,e2,io2) ->
        if (t1,e1) = (t2,e2) 
        then
          Pervasives.compare (ii1.ioid,io1.ioid) (ii2.ioid,io2.ioid)
        else
          Pervasives.compare (t1,e1) (t2,e2)
    | _,_ -> assert false
  else
    eqp


(* ***************************************************************** *)
(* miscellaneous auxiliary functions                                 *)
(* ***************************************************************** *)

exception Transitive
let rec option_map f xs = 
  match xs with 
  | [] -> [] 
  | x::xs -> 
      ( match f x with 
      | None -> option_map f xs 
      | Some x -> x :: (option_map f xs) ) 

let transitive_reduction eq r = 
  let transitive_pairs = 
    List.flatten 
      (List.map 
         (fun (a1,a2) -> 
           option_map (fun (a1',a2') -> if eq a2 a1' then Some (a1,a2') else None) r)
         r) in
  (* a partial check for cycles *)
  if List.exists (fun (a1,a2)->eq a1 a2) (r @ transitive_pairs) then 
    raise Transitive;
  List.filter (fun (a1,a2) -> not (List.exists (function (b1,b2) -> (eq a1 b1 && eq a2 b2)) transitive_pairs)) r 
