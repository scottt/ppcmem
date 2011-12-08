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

(*(*open Printf*)*)

module Make(ISO:Iso.S) =
  struct
    module A = ISO.A

    open MachineDefUtils
    open MachineDefFreshIds
    open MachineDefValue
    open MachineDefTypes
    open MachineDefInstructionSemantics
    open MachineDefStorageSubsystem
    open MachineDefThreadSubsystem
    open Types
    open Model_aux
    open Interact

(* Options *)
let opts = Globals.get_our_runopts ()

(* Utility functions on sets *)
let map_set (f:'a -> 'b) (xs: 'a Pset.set) : 'b Pset.set =
  Pset.fold
    (fun x k ->
      Pset.add (f x) k)
    xs (Pset.empty compare)

let cross_prod_set : 'a Pset.set -> 'b Pset.set -> ('a * 'b) Pset.set =
  fun xs ys ->
    Pset.fold
      (fun x k ->
	Pset.fold
	  (fun y k' ->
	    Pset.add (x,y) k')
	  ys k)
      xs (Pset.empty compare)



    let rec find_last_reg_write same_allowed ts tid i r i' =
      if same_allowed && Pset.exists (fun r' -> r = r') i.regs_out
      then 
	if is_determined (val_written_to_register i.behaviour r)
	then 
	  Some (Register_read_prev (tid,i,r,i)) 
	else
	  (* The register-read cannot be done yet because the writer has
	     not resolved its data *)
          None
      else
        match i'.prev with
	| None -> (* No register write in po-prefix *)
	    Some (Register_read_initial (tid,i,r)) 
	| Some (i'',_) ->
	    begin
	      try
		let iprev = 
		  Pset.choose 
		    (Pset.filter
		       (fun ip -> ip.ioid = i'')
                       (Pset.union (ts tid).in_flight_instructions
			  (ts tid).committed_instructions)) 
		in
		if Pset.exists (fun r' -> r = r') iprev.regs_out
		then (* iprev is the instruction that will feed this read *)
		  if is_determined (val_written_to_register iprev.behaviour r)
		  then 
		    Some (Register_read_prev (tid,i,r,iprev)) 
		  else
		    (* The register-read cannot be done yet because the writer has
		       not resolved its data *)
		    None 
		else find_last_reg_write same_allowed ts tid i r iprev
	      with Not_found ->
		(* No register write in po-prefix, predecessor aborted *)
		Some (Register_read_initial (tid,i,r))
	    end

    let find_final_reg_state ts tid rlocs = 
      let committed_instrs = (ts tid).committed_instructions in
      let last_instrs = 
        Pset.filter
          (fun ilast ->
            not (Pset.exists
                   (fun isucc ->
                     match isucc.prev with
                     | Some (id,_) -> id = ilast.ioid
                     | _ -> false)
                   committed_instrs))
          committed_instrs in
      let relevant_regs = 
        List.map 
          (fun l -> 
            match l with
            | A.Location_reg (tid',r) -> r
            | _ -> assert false)
          (List.filter
             (fun l -> 
               match l with
               | A.Location_reg (tid',r) -> tid = tid'
               | _ -> false)
             rlocs) in
      let regs_out = 
        Pset.fold
          (fun i k ->
            Pset.union i.regs_out k)
          committed_instrs (Pset.empty compare) in
      let regs_requested = Pset.union regs_out 
          (Pset.from_list Pervasives.compare 
             (List.map ISO.arch_reg_to_reg relevant_regs)) in
      let locs_and_vals = 
        Pset.fold
          (fun r k ->
            let ilast = Pset.choose last_instrs in
            let ioid = ilast.ioid in
            let reg_read_cand = find_last_reg_write true ts tid ilast r ilast in
            match reg_read_cand with
            | Some (Register_read_prev (_,_,_,iw)) ->
                let v = val_written_to_register iw.behaviour r in
                (A.Location_reg (tid,ISO.reg_to_arch_reg r),ISO.value_to_arch_value v) :: k
            | Some (Register_read_initial _) ->
                let v = ((ts tid).initial_register_state r) in
                (A.Location_reg (tid,ISO.reg_to_arch_reg r),ISO.value_to_arch_value v) :: k
            | _ -> k)
          regs_requested [] in
      locs_and_vals
      
let find_reg_read_candidates 
    (ts : (thread_id -> thread_state))
    (in_flights : (thread_id * instruction_instance) Pset.set)
    = 
  let reg_reads =
    Pset.filter
      (fun (tid,i) -> 
	reg_read_cand i.behaviour) in_flights in
  let cands = 
    Pset.fold 
      (fun (tid,i) k ->
        let r = reg_to_read i.behaviour in
        match find_last_reg_write false ts tid i r i with
        | Some a -> Pset.add a k
        | None -> k
      ) reg_reads
      (Pset.empty compare) in
  Pset.filter
    (fun m ->
      match m with
      | Register_read_prev (tid,i1,r,i2) -> 
            let b = register_read_prev_cand (ts tid) i1 r i2 in
            let () = if not b then ()(*Warn.warn_always "Bad read (previous reg) candidate"*) else () in 
            b
      | Register_read_initial (tid,i,r) ->
            let b = initial_register_read_cand (ts tid) i r in
            let () = if not b then ()(*Warn.warn_always "Bad read (initial reg) candidate"*) else () in
            b
        | _ -> assert false) 
    cands

let is_reg_or_branch i =
  not
    (i.is_load || i.is_store ||
    i.is_load_reserve || i.is_store_conditional ||
    i.is_load_acquire || i.is_store_release ||
    is_memory_barrier i)    

let find_commit  s =
  let tids = s.storage_subsystem.threads in 
  let is =
    Pset.fold
      (fun tid k ->
        let t = s.thread_states tid in
	let my_ins = t.in_flight_instructions in
        Pset.fold
          (fun i k ->
            if
              is_reg_or_branch i &&
              commit_cand s.model.t (s.thread_states tid) i
            then Commit_reg_or_branch (tid,i)::k
            else k)
          my_ins k)
      tids [] in
  List.fold_left
    (fun k x -> Pset.add x k) (Pset.empty compare) is
          

let find_mem_read_reserve_candidates 
    (ts : (thread_id -> thread_state))
    (m : model_params)
    (s : storage_subsystem_state)
    (ws : write Pset.set)
    (in_flights : (thread_id * instruction_instance) Pset.set)
    = 
  let mem_reads =
    Pset.filter
      (fun (tid,i) -> 
	mem_read_reserve_cand i.behaviour) in_flights in
  Pset.fold 
    (fun (tid,i) tk ->
      let a = mem_loc_to_read i.behaviour in
      let r = read_reserve_request_of tid i.ioid i.behaviour in
      if memory_read_reserve_storage_cand m.t (ts tid) i a 
      then
       (* Potential memory read to do. Find a write to forward, 
          and find possible writes from the storage subsystem *)
        let mem_cands = 
          map_set
            (fun w -> Read_from_storage_subsystem (tid,i,w)) (* was Read_reserve_from_storage_subsystem *)
            (Pset.filter
               (fun w ->
                 send_read_response_cand (*send_read_reserve_response_cand*) m.ss s tid r w)
               ws)
        in
	Pset.union mem_cands tk
      else 
	tk
    ) mem_reads
    (Pset.empty compare)

let find_mem_read_candidates 
    (ts : (thread_id -> thread_state))
    (m : model_params)
    (s : storage_subsystem_state)
    (ws : write Pset.set)
    (in_flights : (thread_id * instruction_instance) Pset.set)
    = 
  let mem_reads =
    Pset.filter
      (fun (tid,i) -> 
	mem_read_cand i.behaviour) in_flights in
  Pset.fold 
    (fun (tid,i) tk ->
      let a = mem_loc_to_read i.behaviour in
      let r = read_request_of tid i.ioid i.behaviour in
      if memory_read_storage_cand m.t (ts tid) i a 
      then
       (* Potential memory read to do. Find a write to forward, 
          and find possible writes from the storage subsystem *)
        let mem_cands = 
          map_set
            (fun w -> Read_from_storage_subsystem (tid,i,w))
            (Pset.filter
               (fun w ->
                 send_read_response_cand m.ss s tid r w)
               ws)
        in
	let rec find_mem_write i' = 
	  match i'.prev with
	  | None -> (* No memory write in po-prefix *)
	      (Pset.(union) mem_cands tk)
	  | Some (i'',_) ->
	      begin
		try
		  let iprev = 
		    Pset.choose 
		      (Pset.filter
			 (fun ip -> ip.ioid = i'')
			 (Pset.(union) (ts tid).in_flight_instructions
			    (ts tid).committed_instructions)) 
		  in
		  if possibly_writes_to_address iprev.behaviour a
		  then (* iprev is the instruction that will possibly feed this read *)
		    let ws = mem_writes_of tid iprev.ioid iprev.behaviour in
		    let det_ws = 
		      Pset.filter 
		        (fun w ->
			  is_determined w.w_addr &&
			  Pervasives.compare w.w_addr a = 0 &&
			  is_determined w.w_value)
			ws in
		    if Pset.is_empty det_ws 
		    then 
		      (* The writer has not resolved its data/address *)
                      (Pset.(union) mem_cands tk)
                    else 
                      if Pset.mem iprev (ts tid).committed_instructions
                      then
                      (* Forwarding write is already committed, just use the last write seen by the thread *)
		      (Pset.(union) mem_cands tk)
		      else
                        (* Forward that *)
		        let w' = Pset.choose det_ws in
                        let b = memory_read_forward_cand m.t (ts tid) i w' iprev in
                        if not b 
                        then 
                          begin 
                            (*Warn.warn_always "Bad read (forwarded) candidate" ; *)
                            tk
                          end 
                        else 
                          (Pset.add (Write_forward_to_read (tid,i,w',iprev)) (Pset.(union) mem_cands tk))
		  else find_mem_write iprev
		with Not_found ->
		  (* No memory write in po-prefix, predecessor aborted *)
		  (Pset.union mem_cands tk)
	      end
	in
	find_mem_write i
      else 
	(tk)
    ) mem_reads
    (Pset.empty compare)

let find_candidates : system_state -> system_state * trans set =
  fun s ->
    let ids = s.id_state in
    let thread_ids = s.storage_subsystem.threads in 
  let ws = s.storage_subsystem.writes_seen in
  let bs = barriers_seen s.storage_subsystem in 
  let ins =
    Pset.fold
      (fun tid k ->
	let t = s.thread_states tid in
	let my_ins = t.in_flight_instructions in
	let my_ins_set =
	  map_set
	    (fun i -> (tid,i)) my_ins in
	Pset.union my_ins_set k)
      thread_ids (Pset.empty compare)
  in 
  let mem_cands = 
        Pset.union (find_mem_read_candidates s.thread_states s.model s.storage_subsystem ws ins)
                   (find_mem_read_reserve_candidates s.thread_states s.model s.storage_subsystem ws ins) in
  (s,
  List.fold_left
    Pset.union
    (Pset.empty trans_compare)
    (mem_cands ::
     find_reg_read_candidates s.thread_states ins ::
     [Pset.fold
       (fun (t,i) k -> 
	 if i.is_store then 
	   let wreqs = 
               mem_writes_of t i.ioid i.behaviour in
	   Pset.add (Commit_write (t,i,wreqs)) k else
           if is_memory_barrier i then 
             let breq = 
	       Pset.choose (barriers_of t i.ioid i.behaviour) in
             Pset.add (Commit_barrier (t,i,breq)) k else
	     if i.is_store_conditional then
	       let wcreq = 
		 Pset.choose (mem_write_conditionals_of t i.ioid i.behaviour) in
               let wcprevs =
                 previous_writes_for_write_conditional (s.thread_states t) i wcreq in
	       let success_possible = 
                 if Pset.is_empty wcprevs then false else
                 accept_wcond_success_cand s.storage_subsystem {wc_w = wcreq; wc_wprev = Some (Pset.choose wcprevs)} in
	       let failing =
                 if Pset.is_empty wcprevs then
                   Pset.add (Commit_write_conditional (t,i,{wc_w = wcreq; wc_wprev = None},false)) k
                 else 
                   Pset.add (Commit_write_conditional (t,i,{wc_w = wcreq; wc_wprev = Some (Pset.choose wcprevs)},false)) k in
               if success_possible then
                 Pset.add (Commit_write_conditional (t,i,{wc_w = wcreq; wc_wprev = Some (Pset.choose wcprevs)},true)) failing else failing 
             else
	       if i.is_load then
		 Pset.add (Commit_read (t,i,i.read_responses)) k else
		 Pset.add (Commit_reg_or_branch (t,i)) k)
       (Pset.filter
	  (fun (t,i) -> 
            commit_cand s.model.t (s.thread_states t) i)
	  ins) (Pset.empty compare);
     map_set
       (fun (t,i) -> Partial_evaluate (t,i))
       (Pset.filter
	  (fun (t,i) -> partial_evaluate_cand (s.thread_states t) i)
	  ins);
(***)
     map_set
       (fun (w,t) -> Write_propagate_to_thread (w,t))
       (Pset.filter
	  (fun (w,t) -> write_announce_cand s.model.ss s.storage_subsystem w t)
	  (cross_prod_set ws thread_ids));
     map_set
       (fun w -> Write_reaching_coherence_point w)
       (Pset.filter
	  (fun w -> write_reaching_coherence_point_cand s.model.ss s.storage_subsystem w)
	  ws);
     map_set
       (fun (w1,w2) -> Partial_coherence_commit (w1,w2))
       (Pset.filter
     	  (fun (w1,w2) -> (not (Pset.mem (w1,w2) s.storage_subsystem.coherence)) && coherence_commitment_cand s.model.ss s.storage_subsystem w1 w2)
     	  (cross_prod_set ws ws));
     map_set
       (fun b -> Acknowledge_sync b)
       (Pset.filter
	  (fun b -> acknowledge_sync_barrier_cand s.model.ss s.storage_subsystem b)
	  bs);
     map_set
       (fun (b,t) -> Barrier_propagate_to_thread (b,t))
       (Pset.filter
	  (fun (b,t) -> barrier_propagation_cand s.model.ss s.storage_subsystem b t)
	  (cross_prod_set bs thread_ids));
   ]))


let do_trans_action : trans -> system_state -> system_state =
  fun t s -> 
    let () = if !Globals.debug > 1 then begin end(*Printf.printf "Doing transition %s\n" (Pp.pp_trans t); flush stdout*) in
    match t with
    | Commit_write (tid,i,wreqs) ->
	let ist = s.id_state in
	let storage_subsystem' =
	    Pset.fold
	      (fun w k -> 
		accept_local_write_action s.model.ss k w)
	      wreqs s.storage_subsystem
	in
        let new_ts,ist' = 
          commit_action s.model.t (s.thread_states tid) i 
            (Pset.fold (fun w k -> Pset.add (Wreq w) k) wreqs (Pset.empty compare))  
            (Pset.empty compare) (Pset.empty compare) (Pset.empty compare)
            ist
        in
	 {s with id_state = ist';
	  storage_subsystem = storage_subsystem';
	 thread_states = 
	 (fun tid' -> 
	   if tid' = tid
	   then new_ts
	   else s.thread_states tid')}
    | Commit_write_conditional (tid,i,wcreq,succ) ->
	let ist = s.id_state in
	let storage_subsystem' =
            if succ then
               accept_wcond_success_action s.storage_subsystem wcreq
            else
               accept_wcond_failure_action s.storage_subsystem wcreq
	in
        let new_ts,ist' = 
          commit_action s.model.t (s.thread_states tid) i 
            (Pset.empty compare) (Pset.empty compare) 
            (Pset.singleton compare (WCreq wcreq))
            (Pset.singleton compare (WCresp succ))
            ist
        in
	 {s with id_state = ist';
	  storage_subsystem = storage_subsystem';
	 thread_states = 
	 (fun tid' -> 
	   if tid' = tid
	   then new_ts
	   else s.thread_states tid')}
    | Commit_barrier (tid,i,breq) ->
	let ist = s.id_state in
	let storage_subsystem' =
	  accept_local_barrier_action s.storage_subsystem breq
	in
        let new_ts,ist' = 
          commit_action s.model.t (s.thread_states tid) i 
	    (Pset.empty compare) (Pset.singleton compare (Breq breq))
            (Pset.empty compare) (Pset.empty compare)
            ist
        in
	 {s with id_state = ist';
	  storage_subsystem = storage_subsystem';
	 thread_states = 
	 (fun tid' -> 
	   if tid' = tid
	   then new_ts
	   else s.thread_states tid')}
    | Commit_read (tid,i,_)
    | Commit_reg_or_branch (tid,i) ->
	let ist = s.id_state in
        let new_ts,ist' = 
          commit_action s.model.t (s.thread_states tid) i 
	    (Pset.empty compare) (Pset.empty compare)
            (Pset.empty compare) (Pset.empty compare)
            ist
        in
	 {s with id_state = ist';
	 thread_states = 
	 (fun tid' -> 
	   if tid' = tid
	   then new_ts
	   else s.thread_states tid')}
    | Write_reaching_coherence_point w -> 
	 {s with
	 storage_subsystem = write_reaching_coherence_point_action s.model.ss s.storage_subsystem w}
    | Write_propagate_to_thread (w,tid) -> 
	 {s with 
	  storage_subsystem = write_announce_action s.storage_subsystem w tid}
    | Partial_coherence_commit (w1,w2) ->
    	 {s with
    	 storage_subsystem = coherence_commitment_action s.storage_subsystem w1 w2}
    | Barrier_propagate_to_thread (b,tid) ->
    	 {s with
    	 storage_subsystem = barrier_propagation_action s.model.ss s.storage_subsystem b tid;
    	}
    | Read_from_storage_subsystem (tid,ir,w) ->
        let r = read_request_of tid ir.ioid ir.behaviour in
        let new_ts = memory_read_action (s.thread_states tid) ir r w 
        in
    	 {s with
	  storage_subsystem = 
	  send_read_response_action s.model.ss s.storage_subsystem tid ir w;
    	 thread_states =
    	 (fun tid' ->
    	   if tid' = tid
    	   then new_ts
    	   else s.thread_states tid')}
(*    | Read_reserve_from_storage_subsystem (tid,ir,w) ->
        let r = read_reserve_request_of tid ir.ioid ir.behaviour in
        let new_ts = memory_read_action (s.thread_states tid) ir r w 
        in
    	 {s with
	  storage_subsystem = 
	  send_read_reserve_response_action s.storage_subsystem tid ir w;
    	 thread_states =
    	 (fun tid' ->
    	   if tid' = tid
    	   then new_ts
    	   else s.thread_states tid')} *)
    | Write_forward_to_read (tid,ir,w,iw) ->
       (* let () = if !Globals.debug > 1 then Printf.printf "Forwarding write\n" in*)
        let r = read_request_of tid ir.ioid ir.behaviour in
        let new_ts = memory_read_action (s.thread_states tid) ir r w 
        in
    	 {s with
    	 thread_states =
    	 (fun tid' ->
    	   if tid' = tid
    	   then new_ts
    	   else s.thread_states tid')}
    | Acknowledge_sync b -> 
        let new_ts,ist' = accept_sync_barrier_ack_action s.model.t (s.thread_states b.b_thread) {br_thread = b.b_thread; br_ioid = b.b_ioid; br_eiid = b.b_eiid} s.id_state
        in
	 {s with id_state = ist';
	  storage_subsystem = acknowledge_sync_barrier_action s.model.ss s.storage_subsystem b;
	 thread_states =
	 (fun tid ->
	   if tid = b.b_thread
	   then new_ts
	   else s.thread_states tid)}
    | Register_read_prev (tid,ir,r,iw) -> 
        let new_ts = 
	  let v = val_written_to_register iw.behaviour r in
	  register_read_action (s.thread_states tid) ir v
        in
	 {s with
	 thread_states = 
	 (fun tid' -> 
	   if tid' = tid
	   then new_ts
	   else s.thread_states tid')}
    | Register_read_initial (tid,ir,r) -> 
        let new_ts = 
          let v = ((s.thread_states tid).initial_register_state r) in
	  register_read_action (s.thread_states tid) ir v
        in
	 {s with
	 thread_states = 
	 (fun tid' -> 
	   if tid' = tid
	   then new_ts
	   else s.thread_states tid')}
    | Partial_evaluate (tid,i) ->
        let new_ts = partial_evaluate_action (s.thread_states tid) i in
	 {s with
	 thread_states = 
	 (fun tid' -> 
	   if tid' = tid
	   then new_ts
	   else s.thread_states tid')}

let find_eager_candidates =
  fun s -> 
    let thread_ids = s.storage_subsystem.threads in
    let bs = barriers_seen s.storage_subsystem in 
    let ins =
    Pset.fold
      (fun tid k ->
	let t = s.thread_states tid in
	let my_ins = t.in_flight_instructions in
	let my_ins_set =
	  map_set
	    (fun i -> (tid,i)) my_ins in
	Pset.union my_ins_set k)
      thread_ids (Pset.empty compare)
    in
    List.fold_left
      Pset.union
      (Pset.empty trans_compare)
      (find_reg_read_candidates s.thread_states ins ::
       (if opts.Globals.fast then
         fun k -> find_commit s::k
       else
         fun k -> k)
       [map_set
         (fun (t,i) -> Partial_evaluate (t,i))
         (Pset.filter
	    (fun (t,i) -> partial_evaluate_cand (s.thread_states t) i)
            ins);
        ])

let rec do_eager_trans s =
  let cands = find_eager_candidates s in
  if Pset.is_empty cands
  then s
  else 
    let snew =
      Pset.fold
        (fun t sk ->
	  let s' = do_trans_action t sk in
	  s')
        cands s
    in
    do_eager_trans snew

  end
