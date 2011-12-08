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

module Make :
  functor (ISO : Iso.S) ->
    sig
      (* 
       * Find the final register state at a thread
       * Inputs: thds -- all the thread states (as a map from thread_id)
       *         tid  -- thread id to consider
       * Output: s -- list of all register locations written to by this thread
       *              paired with the last value written there
       *)
      val find_final_reg_state :
        (MachineDefTypes.thread_id -> MachineDefTypes.thread_state) ->
        MachineDefTypes.thread_id -> 
          ISO.A.location list -> 
            (ISO.A.location * ISO.A.V.v) list

      (* 
       * Find the set of enabled transitions
       * Input: st -- state we are looking at
       * Output: st' -- possibly modified state (for functional fresh id 
       *                generation, id-state might have been modified)
       *   and   ts  -- set of all enabled transitions
       *)
      val find_candidates :
        MachineDefTypes.system_state ->
        MachineDefTypes.system_state *
        MachineDefTypes.trans MachineDefThreadSubsystem.set

      (* 
       * Perform a transition
       * Inputs: tr -- transition to do
       *         st -- current state
       * Output: st' -- state after transition
       *)
      val do_trans_action :
        MachineDefTypes.trans ->
        MachineDefTypes.system_state -> MachineDefTypes.system_state

      (* 
       * Find all the transition candidates that can be done eagerly
       * (internal computation and register reads)
       * Input : st -- current state
       * Output : ts -- set of allowed eager transitions
       *)
      val find_eager_candidates :
        MachineDefTypes.system_state -> MachineDefTypes.trans Pset.set

      (* 
       * Do all the possible eager transitions
       * Input : st -- current state
       * Output : st' -- state after doing as many eager transitions as
       *                 possible (possibly zero)
       *)
      val do_eager_trans :
        MachineDefTypes.system_state -> MachineDefTypes.system_state
    end
