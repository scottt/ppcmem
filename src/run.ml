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

open Printf
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

module Make (ISO:Iso.S) =
  struct

    module TR = Transitions.Make(ISO)
    open TR
    module A = ISO.A
    module C = Constraints.Make(A)
    open ConstrGen
    module T = Test.Make(A)

    (* ************************************************************** *)
    (* debug print of a test                                          *)
    (* ************************************************************** *)

    let pp_test_debug test = 
      let module Dump =
        SimpleDumper.Make
          (struct
	    module A = A
	    module C = Constraints.Make (A)
		
	    type state = A.state
	    let dump_state chan st = fprintf chan "%s" (A.dump_state st)
		
	    type constr = C.constr
	    let dump_constr = C.dump_constraints
		
	    type location = A.location
	    let dump_location chan loc = fprintf chan "%s" (A.pp_location loc false)
          end) in
      let t = {
        MiscParser.init = test.Test.init_state ;
        info = test.Test.info ;
        prog = test.Test.nice_prog ;
        condition = T.find_our_constraint test;
        locations = [] ;
        kinds = [] ;
      } in
      Dump.dump stdout test.Test.name t        


        (* ************************************)
        (* construct initial state for a test *)
        (* ************************************)

    let initial_state_of_test test model = 
      let init_thread = 1000 in
      let init_ioid = 1000 in
      let upto_set k =
        let rec upto_set' n k = 
	  if n < 0 then k 
	  else upto_set' (n - 1) (Pset.add n k)
        in 
        upto_set' k (Pset.empty Pervasives.compare)
      in
      (* subtract 1 to start from zero *)
      let tids = upto_set ((List.length test.Test.nice_prog) - 1) in
      let irv t r = 
        try ISO.arch_value_to_value (A.look_in_state test.Test.init_state
	                               (A.Location_reg ((ISO.thread_id_to_arch_proc t),(ISO.reg_to_arch_reg r)))) 
        with A.LocUndetermined -> assert false
      in
      let init_writes = 
        List.filter
	  (fun (l,v) ->
	    match l with
	    | A.Location_global _ -> true
	    | _ -> false) 
	  (A.state_to_list test.Test.init_state) in
      let implicit_init_writes =
	let mentioned_locs = 
          (List.map 
             (fun (_,l) -> A.Location_global l)
             (List.filter
	        (fun (l,v) ->
	          match v with
                    (* If you use integer locations, this won't work (such as location 0).
                       Not sure how to fix in the general case without doing actual constraint
                       solving. *)
	          | A.V.Val (SymbConstant.Symbolic _) -> true
	          | _ -> false)
	        (A.state_to_list test.Test.init_state))) in
        let mentioned_locs = Misc.rem_dups A.location_equal (List.sort A.location_compare mentioned_locs) in
        List.map
          (fun l -> (l,A.V.intToV 0))
          (List.filter
             (fun l ->
               if !Globals.debug >= 0 then
                 Printf.printf "LOC: %s" (A.pp_location l false); 
               not (List.mem_assoc l init_writes)
             )
             mentioned_locs) in
      let init_write_events0 = 
	List.fold_right
	  (fun (l,v) wk ->
	    match l with
	    | A.Location_global a ->
		let w = make_write_event init_thread init_ioid (ISO.arch_value_to_value a) (ISO.arch_value_to_value v)
		in 
		w::wk
	    | _ -> assert false)
	  (init_writes @ implicit_init_writes) [] in
      let init_write_events = List.sort (fun w1 w2 -> Pervasives.compare w1.w_addr w2.w_addr) init_write_events0 in
      let init_write_event_ids = List.map (fun w -> w.w_eiid) init_write_events in
      let () = Globals.init_write_event_ids := init_write_event_ids in
      if !Globals.debug >= 0 then begin
        Printf.printf "TOP init write ids %s\n"
          (String.concat ","
             (List.map
                (fun e ->
                  Printf.sprintf "<%d:%d>W %s %s"
                    e.weiid_thread e.weiid_ioid (Pp.pp_value e.weiid_addr)
                    (Pp.pp_value e.weiid_value))
                (!Globals.init_write_event_ids)))
      end ;
      let initial_state = 
        MachineDefSystem.initial_system_state tids irv init_write_events model in
      let () = 
	if !Globals.debug > 1 then 
          printf "Initially...\n%s**********\n" 
            (let ui_state = Pp.make_ui_system_state_opt None initial_state [] in 
            Printf.sprintf "\n%s\n" (Pp.pp_ui_system_state (Globals.get_our_ppmode ()) ui_state)) in
      (*(Pp.pp_system_state None initial_state TR.itd_empty) in*)

      initial_state



    (* ************************************************************** *)
    (* initial fetching of instructions                               *)
    (* ************************************************************** *)

    module Imap =
      Map.Make
        (struct type t = string let compare = String.compare end)
	
    let is_back_jump addr_jmp tgt = match tgt with
    | [] -> false
    | (addr_tgt,_)::_ -> SymbConstant.compare addr_jmp addr_tgt >= 0


    type 'a idstate_monad = id_state -> ('a * id_state)

    let returnM : 'a -> 'a idstate_monad = fun x idst -> (x,idst)

        (* sequence *)
    let (>>=) : 'a idstate_monad -> ('a -> 'b idstate_monad) -> 'b idstate_monad =
      fun aM bM idst -> 
        let (a,idst1) = aM idst in
        let (b,idst2) = bM a idst1 in
        (b,idst2)

    let genM : ioid idstate_monad = gen_ioid 

        
	
    let fetch_instructions (p:A.program) (starts0:A.start_points) s =
      let starts = List.rev starts0 in
      let fetch_code seen addr_jmp lbl = 
	let lbl = match lbl with
	  SymbConstant.Symbolic s -> s
	| SymbConstant.Concrete _ -> Warn.user_error "numeric labels not allowed"
	in
        let tgt =
          try A.LabelMap.find lbl p
	  with Not_found ->
	    Warn.user_error
	      "Segmentation fault (kidding, label %s not found)" lbl in        
        if is_back_jump addr_jmp tgt then
	  Warn.user_error "Do not handle loops"
        else
          (tgt,seen) in

      let rec add_next_instr 
          (proc : thread_id) 
          (prog_order : A.program_order_index) 
          seen 
          (addr : address) 
          (inst : A.I.arch_instruction) 
          (prev : (ioid * reaches_by) option)
          (nexts : A.code) 
          : instruction_instance Pset.set idstate_monad =
        genM
          >>=
        (fun ii ->
          (starting_inst_instance ii (ISO.arch_instr_to_instr inst) addr prev) 
            >>=
          (fun inst_instance -> 
            (next_instr proc (A.next_po_index prog_order) seen addr nexts inst_instance)
              >>=
            (fun rest ->
              returnM (Pset.add inst_instance rest))))

      and add_lbl proc prog_order seen addr_jmp prev lbl =
        let (code,seen) = fetch_code seen addr_jmp lbl in
      	add_code proc prog_order seen prev code 

      and next_instr proc prog_order seen addr nexts inst_instance = 
	match next_fetch_addr inst_instance.behaviour with
	| Next -> add_code proc prog_order seen (Some (inst_instance.ioid,Always)) nexts
	| Jump_to lbl ->
	    let addr_c = 
	      match addr with
	      | Rigid c -> ISO.const_to_arch_const c 
	      | _ -> Warn.user_error "Flexible branch target" in
      	    add_lbl proc prog_order seen addr_c (Some (inst_instance.ioid,Always)) (ISO.const_to_arch_const lbl)
	| Cond_branch_to (v,lbl) ->
	    let addr_c =
	      match addr with
	      | Rigid c -> ISO.const_to_arch_const c 
	      | _ -> Warn.user_error "Flexible branch target" in
            (add_lbl proc prog_order seen addr_c (Some (inst_instance.ioid,IfNonZero v)) (ISO.const_to_arch_const lbl))
              >>= 
            (fun iset1 -> 
              (add_code proc prog_order seen (Some (inst_instance.ioid,IfZero v)) nexts)
                >>=
              (fun iset2 ->
	        returnM (Pset.union iset1 iset2)))

      and add_code 
          (proc : thread_id) 
          (prog_order : A.program_order_index) 
          seen 
          (prev : (ioid * reaches_by) option) 
          (nexts : A.code) : instruction_instance Pset.set idstate_monad =
        match nexts with
        | [] -> returnM (Pset.empty compare)
        | (addr,inst)::nexts ->
      	    add_next_instr proc prog_order seen (ISO.arch_value_to_value (A.V.Val addr)) inst prev nexts

      in

      let jump_start 
          (proc : thread_id) 
          (code : A.code) : instruction_instance Pset.set idstate_monad =
        add_code proc  A.zero_po_index Imap.empty None code in

      let add_instrs_for_a_processor (proc,code) (k,ist) =
      	let (instrs_proc,ist') = jump_start proc code ist in
      	((proc,instrs_proc) :: k,ist') in

      let list_of_all_instr_sets ist =
      	List.fold_right
      	  add_instrs_for_a_processor
      	  starts
      	  ([],ist) in

      let (instrs_by_proc,ist') = list_of_all_instr_sets s.id_state in
      let initial_state_fetched = 
        List.fold_right
      	  (fun (proc,instrs) s ->
      	    { s with thread_states =
      	      (fun tid ->
      	        let ts = s.thread_states tid in
      	        if tid = proc
      	        then {ts with
      		      in_flight_instructions = instrs}
      	        else ts)})
      	  instrs_by_proc ({s with id_state = ist'}) in
      let () = 
	if !Globals.debug > 1 then 
          printf "Initially...\n%s**********\n" 
            (let ui_state = Pp.make_ui_system_state_opt None initial_state_fetched [] in 
            Printf.sprintf "\n%s\n" (Pp.pp_ui_system_state (Globals.get_our_ppmode ()) ui_state)) in

      let _ = Pp.initialise_pretty_event_map !Globals.init_write_event_ids initial_state_fetched in

      (*(Pp.pp_system_state None initial_state_fetched TR.itd_empty) in*)
      initial_state_fetched



    (* ************************************************************** *)
    (* main exploration code                                          *)
    (* ************************************************************** *)

    (* Statistics *)

    type count_t =
        {
         total : int ;            (* all view orders *)
         got_stuck : bool;        (* whether deadlocked *)
         finals : A.StateSet.t ;    (* All final states *)
       }

    exception Count of count_t

    let count_start =
      { total=0 ; 
        got_stuck = false;
        finals = A.StateSet.empty; 
      }

    let count_merge c1 c2 =
      { total = c1.total + c2.total ;
        got_stuck = c1.got_stuck || c2.got_stuck ;
        finals = A.StateSet.union c1.finals c2.finals; 
  }



        
    module System_State_Set =
      Set.Make(
      struct
        type t = system_state
        let compare = system_state_compare
      end)
        
    type system_state_set = System_State_Set.t

    module LocSet =
      MySet.Make
        (struct
          type t = A.location
          let compare = A.location_compare
        end)


        (* List of relevant locations*)
    let outcome_locations t =
      let c = T.find_our_constraint t in
      let locs = LocSet.of_list t.Test.flocs in
      let locs =
        ConstrGen.fold_constr
          (fun (loc,_v) r -> LocSet.add loc r)
          c locs in
      LocSet.elements locs

        (* List of relevant registers *)
    let regs_of_locs locs =
      List.filter
        (fun l -> 
          match l with
          | A.Location_reg _ -> true
          | _ -> false)
        locs

    let check_constr c state = 
      match c with
      | ForallStates p
      | QueryState p
      | ExistsState p -> C.check_prop p state
      | NotExistsState p -> not (C.check_prop p state)

    let match_constr c state =
      match c with
      | ForallStates p
      | QueryState p
      | ExistsState p
      | NotExistsState p -> C.check_prop p state

    let is_existential c = 
      match c with
      | QueryState _ | ExistsState _ -> true
      | ForallStates _| NotExistsState _ -> false

    let is_universal c = not (is_existential c)

    let find_constraints m c s =
      let p = 
        match c with
        | ForallStates p
        | QueryState p
        | ExistsState p
        | NotExistsState p -> p in
      let fs_expected =
        let rec prop_to_state p =
          match p with
          | And (pl) ->
              List.fold_right
                (fun p k -> prop_to_state p @ k)
                pl [] 
          | Atom (l,v) -> [(l,v)]
          | _ -> failwith "Constraint type not supported"
        in
        prop_to_state p
      in
      let reg_reads,mem_reads =
        List.partition
          (fun (l,v) -> match l with A.Location_reg _ -> true | _ -> false) fs_expected in
      let read_constrs =
        List.fold_right
          (fun c k ->
            match c with
            | (A.Location_reg (p,r),v) ->
                let p_instrs = (s.thread_states p).in_flight_instructions in
                let rec mem_read_feeding_reg_in_prefix i r =
                  if Pset.exists (fun r' -> r = r') i.regs_out 
                  then
                    begin
                      let v_r = val_written_to_register i.behaviour r in
                      match v_r with
                      | Rigid _ -> None (* Last register write a constant : no constraint (possible future optimisation, check if it is the right value) *)
                      | Flexible fs ->
                          begin
                            let feeding_uop =
                              try
                                Some 
                                  (List.find 
                                     (fun a ->
                                       match a with
                                       | Read_mem (a,Flexible fs') -> fs = fs'
                                       | Read_mem_reserve (a,Flexible fs') -> fs = fs'
                                       | Read_mem_acq (a,Flexible fs') -> fs = fs'
                                       | Read_reg (r,Flexible fs') -> fs = fs'
                                       | _ -> false) 
                                     i.behaviour.remaining)
                              with Not_found -> None 
                            in
                            match feeding_uop with
                            | None -> None (* Probably calculated value : no constraint *)
                            | Some (Read_mem _) -> Some (i.program_loc,(ISO.arch_value_to_value v))
                            | Some (Read_mem_reserve _) -> Some (i.program_loc,(ISO.arch_value_to_value v))
                            | Some (Read_mem_acq _) -> Some (i.program_loc,(ISO.arch_value_to_value v))
                            | Some (Read_reg (r',_)) -> mem_read_feeding_reg_in_prefix i r'
                            | _ -> assert false
                          end
                    end
                  else match i.prev with
                  | None -> None (* Could not find a register write! (will probably read initial state) : no constraint *)
                  | Some (i',_) ->
                      let iprev =
                        Pset.choose 
                          (Pset.filter (fun ip -> ip.ioid = i') p_instrs)
                      in mem_read_feeding_reg_in_prefix iprev r
                in
                let maximal_p =
                  Pset.filter
                    (fun ilast ->
                      not (Pset.exists
                             (fun isucc ->
                               match isucc.prev with
                               | Some (id,_) -> id = ilast.ioid
                               | _ -> false)
                             p_instrs))
                    p_instrs in
                List.fold_right
                  (fun i k ->
                    match mem_read_feeding_reg_in_prefix i (ISO.arch_reg_to_reg r) with
                    | None -> k
                    | Some (l,v) ->
                        begin
                          (l,v) :: k
                        end) 
                  (Pset.elements maximal_p) k
            | _ -> assert false) 
          reg_reads []
      in
      let mem_constrs : (value * value) list =
        List.map
          (fun c ->
            match c with
            | (A.Location_global l,v) -> (ISO.arch_value_to_value l,ISO.arch_value_to_value v)
            | _ -> assert false)
          mem_reads
      in
      read_constrs @ mem_constrs




    (* ************************************************************** *)
    (* imperative per-session state                                   *)
    (* ************************************************************** *)

    let prune_count = ref 0
    let seen_succs = ref 0

    let seen_imp = ref (System_State_Set.empty) 
    let count_seen = ref count_start

    let reset_seen () =
      prune_count := 0 ;
      seen_succs := 0 ;
      seen_imp := System_State_Set.empty ;
      count_seen := count_start


    (* ************************************************************** *)
    (*  body of main "loop" -                                         *)
    (* ************************************************************** *)

    let rec do_trans : Globals.ppmode -> info -> system_state -> (result_type -> unit) -> ((unit -> unit) -> unit) -> (unit -> unit) -> (unit -> unit) -> unit =
      fun m    (* printing information *)
          info (* current state of search *)
          s    (* system state we're looking at *)
          k_success   (* continuation to feed out result to, when no more transitions *)
          k_at_end   (* continuation after done processing the subtree rooted at this state *)
          k_undo     (* continuation to undo one step *)
          k_userquit (* continuation for user-interaction-quit *)
        ->
          let () = if !Globals.debug > 1 then 
            begin
              Printf.printf "My state = %s\n" 
                (let ui_state = Pp.make_ui_system_state_opt None s [] in 
                Printf.sprintf "\n%s\n" (Pp.pp_ui_system_state m ui_state));
              Printf.printf "Do trans, transitions seen so far: %s\n" (String.concat ";" (List.map (Pp.pp_trans m) info.trans_so_far)); flush stdout; 
            end in
          if System_State_Set.mem s !seen_imp
          then 
            begin 
              prune_count := 1 + (!prune_count);  
	      if !Globals.debug > 1  
	      then 
	        begin 	          Printf.printf "Pruning subtree, seen state already\n"; 
	          flush stdout;
	        end; 
              k_at_end k_undo
            end
          else
            begin
              seen_imp := System_State_Set.add s (!seen_imp);
              seen_succs := 1 + (!seen_succs);
              let s_e = do_eager_trans s in
              let s_c,cands = find_candidates s_e in

	      let filtered_cands = 
	        Pset.filter
	          (fun t ->
	            match t with
	            | Read_from_storage_subsystem (tid, ir, w) 
 (*                   | Read_reserve_from_storage_subsystem (tid, ir, w) *)
	            | Write_forward_to_read (tid, ir, w, _) -> 
		        if List.mem_assoc ir.program_loc info.allowed_acts 
		        then w.w_value = (List.assoc ir.program_loc info.allowed_acts)
		        else true
	            | _ -> true) cands in

	      let remove_current_and_undo = 
	        fun () -> 
	          begin
	            seen_imp := System_State_Set.remove s (!seen_imp);
	            k_undo ()
	          end
	      in
              
	      let k_info =
                fun {info=info_i;cands=cands_i} ->
                  
	          let new_undo_fn =
	            fun () ->
		      begin
		        seen_imp := System_State_Set.remove s (!seen_imp);	      
		        Printf.printf "----------------------------------------------------------------------------\n";
		        Printf.printf "Undo to:\n";
		        (* No more follow list after undoing *)
		        let info' = {info with trans_todo = []} in
		        do_trans m info' s k_success k_at_end k_undo k_userquit 
		      end in

                  let info_s = {info_i with last_system_state = Some s_c} in
                  
                  if Pset.is_empty cands_i 
                  then (* Execution done *)
                    let () = k_success {transitions = info.trans_so_far;state = s_c} in k_at_end remove_current_and_undo
                  else 
                    let k_now =
		      Pset.fold
	                (fun t k_next ->
	                  let s' = do_trans_action t s_c in
                          let info' = {info_s with trans_so_far = info_s.trans_so_far @ [t]} in
                          fun undo_fn -> 
		            do_trans m info' s' k_success k_next new_undo_fn k_userquit)
	                cands_i
		        k_at_end
	            in
	            k_now new_undo_fn
	      in
(* It is important to be taill recursive here... (LM) *)
              if not ((Globals.get_our_runopts()).Globals.interactive) then
                k_info {info = info; cands = filtered_cands}
              else
                interact_with_user m s_c
                  {info = info;cands = filtered_cands}
                  k_info remove_current_and_undo k_userquit              
            end

    (* ************************************************************** *)
    (* calculate whether a state is stuck                             *)
    (* ************************************************************** *)

    let is_stuck s =
      let tids = s.storage_subsystem.threads in
      let threads_stuck = 
        Pset.exists 
          (fun tid ->
            not (Pset.is_empty (s.thread_states tid).in_flight_instructions))
          tids
      in
      let addresses = addresses_seen s.storage_subsystem in
      let writes_stuck =
        Pset.exists
          (fun a ->
	    not (is_strict_linear_order (restrict s.storage_subsystem.coherence (writes_at_addr s.storage_subsystem a))))
          addresses ||
          (if s.model.ss.coherence_points then 
            Pset.exists
              (fun w ->
	        not (Pset.mem w s.storage_subsystem.writes_past_coherence_point))
              s.storage_subsystem.writes_seen
          else
            false)
      in
      let barriers_stuck =
        Pset.exists
          (fun tid ->
	    Pset.exists
	      (fun b ->
	        not (List.mem (SBarrier b) (s.storage_subsystem.events_propagated_to tid)))
	      (barriers_seen s.storage_subsystem))
          tids in
      let errorprint_cond =
        (Globals.get_our_runopts ()).Globals.interactive ||
        ((!Globals.debug >= 0) && 
         ( not (Globals.get_our_runopts ()).Globals.statematchmode) &&
         ((Globals.get_our_runopts ()).Globals.safemode)) in
      begin
        if errorprint_cond && threads_stuck then Printf.eprintf "%s\n" (Pp.col_red "Stuck threads") else ();
        if errorprint_cond && writes_stuck then Printf.eprintf "%s\n" (Pp.col_red "Stuck writes") else ();
        if errorprint_cond && barriers_stuck then Printf.eprintf "%s\n" (Pp.col_red "Stuck barriers") else ();
        threads_stuck || writes_stuck || barriers_stuck
      end


    (* ******************************************************************** *)
    (* the top-level of the main search, for the default (-safe -allstates) *)
    (* ******************************************************************** *)

    (* sc is what to do with a state that has no transitions,
       and calc_safe_style is the real top-level *)

    let cutdown_state locs st = 
      let pp_state = 
        List.fold_right
          (fun loc r -> A.state_add r loc (A.look_in_state st loc))
          locs A.state_empty in
      pp_state

    let sc m locs r = 
      let reg_state =
        Pset.fold
          (fun tid k ->
            (find_final_reg_state r.state.thread_states tid (regs_of_locs locs)) @ k)
          r.state.storage_subsystem.threads [] in
      let max_writes =
        maximal_elements (r.state.storage_subsystem.writes_seen) (r.state.storage_subsystem.coherence) in
      let final_state =
        Pset.fold
          (fun w k ->
            A.state_add k (A.Location_global (ISO.value_to_arch_value w.w_addr)) (ISO.value_to_arch_value w.w_value))
          max_writes (A.build_state reg_state) in

      let () = if !Globals.debug > 0 then
        begin
          Printf.printf "Found...\n%s**********\n" 
            (let ui_state = Pp.make_ui_system_state_opt None r.state [] in 
            Printf.sprintf "\n%s\n" (Pp.pp_ui_system_state m ui_state));
          Printf.printf "Transitions: %s\n" (List.fold_left (fun k t -> k ^ ";" ^ Pp.pp_trans m t) "" r.transitions);
          flush stdout
        end
      in
      let c = !count_seen in
      let () = let x = (c.total+1) in if  true (*x mod 1000 = 0*)  then
        begin
          if !Globals.debug >= 0 then
            Printf.printf
              "Found %6d : Prune count=%6d  seen_succs=%6d  %5d states \n"
              x (!prune_count) (!seen_succs)
              (1 + System_State_Set.cardinal (!seen_imp));
        end
      else () in
      let () =
        if (Globals.get_our_runopts ()).Globals.interactive then
          begin 
            let final_pp_state = cutdown_state locs final_state in
            Printf.printf "%s\n" (A.dump_state final_pp_state);
            display (sprintf "<center><b><span style='color:blue'>%s</span></b></center><br/>" (A.dump_state final_pp_state)) "" true 
          end
        else 
          ()
      in
      
      count_seen := 
        {total = c.total + 1;
         got_stuck = if is_stuck r.state then true else c.got_stuck;
         finals = if not (is_stuck r.state) then A.StateSet.add final_state c.finals else c.finals; 
       }; 
      ()


    let calc_safe_style m test s (at_end_fn : (unit -> unit) -> unit) userquit_fn =
      let rec initial_undo_fn =
        fun () ->
          begin
	    Printf.printf "Cannot undo from initial state\n";
	    call_trans ()
          end
      and call_trans = 
        fun () ->
          do_trans m ({trans_todo= !Globals.follow; choices_so_far=[]; trans_so_far = []; last_system_state=None; allowed_acts = []}) s (sc m (outcome_locations test)) at_end_fn initial_undo_fn userquit_fn
      in
      call_trans () 


    (* ******************************************************************** *)
    (* the top-level of the main search, for -quick                         *)
    (* ******************************************************************** *)

    exception Found_soln of system_state

    let sc_quick ar m locs r =
      let c = !count_seen in
      if is_stuck r.state then
        begin
          let () = let x = (c.total+1) in if  true (*x mod 1000 = 0*)  then
	    begin
	      if !Globals.debug >= 0 then
                Printf.printf
                  "Found %6d : Prune count=%6d  seen_succs=%6d  %5d states \n"
                  x (!prune_count) (!seen_succs)
                  (1 + System_State_Set.cardinal (!seen_imp));
	    end in
          () (* continue, ignoring deadlocks TODO look at deadlocks *)
        end
      else 
        begin
          let max_writes =
	    maximal_elements (r.state.storage_subsystem.writes_seen) (r.state.storage_subsystem.coherence) in
          if Pset.for_all
      	      (fun w -> 
                if List.mem_assoc w.w_addr ar 
                then List.assoc w.w_addr ar = w.w_value
                else true)
      	      max_writes 
          then
	    raise (Found_soln r.state)
          else
      	    () (* continue to find another final memory write *)
        end

    let calc_quick_style m test s at_end_fn userquit_fn =
      (* 1. do a straight dfs search to find one complete trace w/o deadlocks *)
      try
        let () = do_trans m ({trans_todo= !Globals.follow; choices_so_far=[]; trans_so_far = []; last_system_state=None; allowed_acts = []}) s (sc_quick [] m (outcome_locations test)) (fun ufn -> ()) (fun () -> ()) userquit_fn in
        () (* normal return = failure to find a final state *)
      with
        Found_soln fs ->
          (* 2. Use the final state thus found to find the reads and writes

	     NOTE: This assumes that all reads and writes in the system
	     always write to the same statically determinable address,
	     i.e. all calculations for address dependency are in fact
	     fake.  TODO: check that at startup. 
           *)
          begin
	    let all_reads = 
	      Pset.filter
	        (fun i -> (Pset.cardinal i.writes_read_from) > 0)
	        (Pset.fold
	           (fun tid k ->
		     Pset.union ((fs.thread_states tid).committed_instructions) k)
	           fs.storage_subsystem.threads
	           (Pset.empty compare_instruction_instance)) in
	    let all_writes = fs.storage_subsystem.writes_seen in
	    let writes_by_loc =
	      Pset.fold
	        (fun w k ->
	          if List.mem_assoc w.w_addr k then
		    let prev = List.assoc w.w_addr k in
		    (w.w_addr, w :: prev) :: (List.remove_assoc w.w_addr k)
	          else (w.w_addr, [w]) :: k)
	        all_writes [] in
	    (* 3. Make up the rfmap possibilities *)
	    let rf_poss = 
	      (* Actual program reads *)
	      List.map 
	        (fun ir ->
	          let wread = Pset.choose ir.writes_read_from in
	          let loc_read = wread.w_addr in
	          let ws = List.assoc loc_read writes_by_loc in
	          List.map 
		    (fun w -> (ir.program_loc,w.w_value))
		    ws)
	        (Pset.elements all_reads) @ 
	      (* The final write at a loc *)
	      List.map
	        (fun (a,ws) ->
	          let ws_poss =
		    if List.length ws <= 1 then ws 
		    else 
		      (* remove init *)
		      List.filter (fun w -> w.w_thread != 1000) ws
	          in
	          List.map (fun w -> (a,w.w_value)) ws_poss) 
	        writes_by_loc
	    in
	    (* 4. Fold a quick search over all the rfmap possibilities *)
	    let () = Misc.fold_cross 
	        rf_poss
	        (fun rf () ->
	          try
	            let () = seen_imp := System_State_Set.empty in
	            let () = do_trans m ({trans_todo= !Globals.follow; choices_so_far=[]; trans_so_far = []; last_system_state=None; allowed_acts = rf}) s (sc_quick rf m (outcome_locations test)) (fun _ -> ()) (fun () -> ()) userquit_fn in
	            () (* normal return = failure to find a final state *)
	          with
	            Found_soln fs ->
		      let reg_state =
		        Pset.fold
		          (fun tid k ->
		            (find_final_reg_state fs.thread_states tid (regs_of_locs (outcome_locations test))) @ k)
		          fs.storage_subsystem.threads [] in
		      let max_writes =
		        maximal_elements (fs.storage_subsystem.writes_seen) (fs.storage_subsystem.coherence) in
		      let final_state =
		        Pset.fold
		          (fun w k ->
		            A.state_add k (A.Location_global (ISO.value_to_arch_value w.w_addr)) (ISO.value_to_arch_value w.w_value))
		          max_writes (A.build_state reg_state) in
		      let c = !count_seen in
		      count_seen := {total = c.total + 1;
			             got_stuck = if is_stuck fs then true else c.got_stuck;
			             finals = if not (is_stuck fs) then A.StateSet.add final_state c.finals else c.finals; 
			           };
		      ()	  
	        ) 
                () 
	    in at_end_fn (fun _ -> ())
          end

    (* ******************************************************************** *)
    (* the top-level of the main search, for -onlystate                     *)
    (* ******************************************************************** *)

    let calc_state_match m test ourconstraint s at_end_fn userquit_fn =
      let allowed = find_constraints m ourconstraint s in
      try do_trans m ({trans_todo= !Globals.follow; choices_so_far=[]; trans_so_far = []; last_system_state=None; allowed_acts = allowed}) s (sc_quick allowed m (outcome_locations test)) at_end_fn (fun () -> ()) userquit_fn
      with Found_soln fs ->
        let reg_state =
          Pset.fold
            (fun tid k ->
              (find_final_reg_state fs.thread_states tid (regs_of_locs (outcome_locations test))) @ k)
            fs.storage_subsystem.threads [] in
        let max_writes =
          maximal_elements (fs.storage_subsystem.writes_seen) (fs.storage_subsystem.coherence) in
        let final_state =
          Pset.fold
            (fun w k ->
              A.state_add k (A.Location_global (ISO.value_to_arch_value w.w_addr)) (ISO.value_to_arch_value w.w_value))
            max_writes (A.build_state reg_state) in
        
        let () = 
          count_seen := {total = 1;
                         got_stuck = false;
                         finals = A.StateSet.singleton final_state; } in
        at_end_fn (fun _ -> ())

    (* ******************************************************************** *)
    (* output of final results                                              *)
    (* ******************************************************************** *)
	
	let warnings = ref ""

    let print_results (m:Globals.ppmode)  test = 
      fun () -> 
        let ourconstraint = T.find_our_constraint test in
        let c = !count_seen in
        let finals = A.StateSet.map (fun st -> cutdown_state (outcome_locations test) st) c.finals in
        (if !Globals.debug > 0 then 
          Printf.printf "Found %d traces\n" c.total);
       (if (String.compare (Globals.pp_ppkind ((Globals.get_our_runopts()).Globals.ppkind)) "Html") == 0 then 
			begin
           result := Printf.sprintf "<b>Test %s %s</b><br/>"
                  test.Test.name.Name.name 
                  (ConstrGen.pp_kind (ConstrGen.kind_of ourconstraint));

           result := !result ^ Printf.sprintf "States %i<br/>" (A.StateSet.cardinal finals);

           result :=!result ^  A.StateSet.pp_str ""
                                 (fun st ->  Printf.sprintf "%s<br/>" (A.dump_state st))
                                  finals ;
			end
        );
        Printf.printf "Test %s %s\n"
          test.Test.name.Name.name
          (ConstrGen.pp_kind (ConstrGen.kind_of ourconstraint));

        Printf.printf "States %i\n" (A.StateSet.cardinal finals);

        A.StateSet.pp stdout ""
          (fun chan st ->  Printf.fprintf chan "%s\n" (A.dump_state st))
          finals ;


        let ok_states,nonok_states =
          A.StateSet.partition
            (fun st -> check_constr ourconstraint st)
            finals in
        begin
          if A.StateSet.cardinal ok_states = 0 && is_existential ourconstraint then begin
            Warn.warn "%s: Existential constraint not satisfied!!!\n"
  	      (Test.simple_name test) ;

       (if (String.compare (Globals.pp_ppkind ((Globals.get_our_runopts()).Globals.ppkind)) "Html") == 0 then 
           warnings := Printf.sprintf "%s: Existential constraint not satisfied!!!<br/>"
  	                             (Test.simple_name test) ;

           result := !result ^ "No (allowed not found)<br/>";
        );
           printf "No (allowed not found)\n";

          end else if
            A.StateSet.cardinal nonok_states > 0 && is_universal ourconstraint
          then begin
            Warn.warn "%s: Universal constraint invalidated"
  	        (Test.simple_name test) ;

       (if (String.compare (Globals.pp_ppkind ((Globals.get_our_runopts()).Globals.ppkind)) "Html") == 0 then 
              warnings := (Printf.sprintf "%s: Universal constraint invalidated<br/>"
  	                                    (Test.simple_name test) );
              result := !result ^ "No (forbidden found)<br/>";
            );
            printf "No (forbidden found)\n";
          end else begin
            printf "Ok\n"; 
			result := !result ^ "Ok<br/>";
          end 
        end;
        printf "Condition " ;
        result := !result ^  "Condition <br/>";
        C.dump_constraints stdout ourconstraint;
        output_char stdout '\n' ;
        result := !result ^  "<br/>";
        if !Globals.auto then begin
          List.iter
            (fun (k,v) ->
              if Misc.string_eq k "Relax" then
                let ok = not (A.StateSet.is_empty ok_states) in
                printf "Relax %s %s %s\n"
                  test.Test.name.Name.name
                  (if ok then "Ok" else "No") v
              else
                fprintf stdout "%s=%s\n" k v)
            test.Test.info
        end else begin
          List.iter
            (fun (k,v) ->
              if Misc.string_eq k "Hash" then
                printf "%s=%s\n" k v;
              result := !result ^ (Printf.sprintf "%s=%s<br/>" k v)
            )
            test.Test.info
        end ;
        let matches,non_matches = 
          A.StateSet.partition
            (fun st -> match_constr ourconstraint st)
            finals in
        let n_matches, n_non_matches = 
          (A.StateSet.cardinal matches), (A.StateSet.cardinal non_matches) in
          Printf.printf "Observation %s %s %d %d %s\n"     
          test.Test.name.Name.name
          (if n_matches = 0 then "Never" else
          if n_non_matches = 0 && (* hack to get state match mode never to say "Always" *) not ((Globals.get_our_runopts()).Globals.statematchmode)
          then "Always" else
            "Sometimes")
          n_matches
          n_non_matches
          (if c.got_stuck then "with deadlocks " else "");
       (if (String.compare (Globals.pp_ppkind ((Globals.get_our_runopts()).Globals.ppkind)) "Html") == 0 then 
        let temp = (Printf.sprintf "Observation %s %s %d %d %s\n"     
          test.Test.name.Name.name
          (if n_matches = 0 then "Never" else
          if n_non_matches = 0 && (* hack to get state match mode never to say "Always" *) not ((Globals.get_our_runopts()).Globals.statematchmode)
          then "Always" else
            "Sometimes")
          n_matches
          n_non_matches
          (if c.got_stuck then "with deadlocks " else "")) in
        result := !result ^ temp ^"<br/>";
		);
        printf "\n%!" ; (* Empty line marks end of output *)
        (**)
        Interact.display !result !warnings false

    (* ******************************************************************** *)
    (* the top-level of the main search, for -onlystate                     *)
    (* ******************************************************************** *)

    let calc_finals (m:Globals.ppmode)  test (s:system_state) = 
      reset_seen () ;
      let ourconstraint = T.find_our_constraint test in
      let userquit_fn = fun () -> () in
      let at_end_fn =
        fun undo_fn ->
          if not ((Globals.get_our_runopts()).Globals.interactive) 
          then 
            print_results m test ()
          else ask_quit_or_undo undo_fn userquit_fn
      in
      let () =
        match ((Globals.get_our_runopts ()).Globals.statematchmode),((Globals.get_our_runopts ()).Globals.safemode) with 
        | (false,true) ->
             calc_safe_style m test s at_end_fn userquit_fn
        | (false,false) ->
             calc_quick_style m test s at_end_fn userquit_fn 
        | (true,true) -> 
             calc_state_match m test ourconstraint s at_end_fn userquit_fn
        | (true,false) -> assert(false) in
      ()
  end

