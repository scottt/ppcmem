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
      module C : Constraints.S with module A = ISO.A

      (* auxiliary to output (to stdout) a debug print of a parsed test *)
      val pp_test_debug : ('a, (int * ISO.A.pseudo list) list, 'b, ISO.A.state,
            (ISO.A.location, ISO.A.V.v) ConstrGen.prop ConstrGen.constr,
            ConstrGen.kind, 'c)
           Test.t -> unit

      (* Auxilary to construct the initial state of a test from a parsed test *)
      val initial_state_of_test : ('a, 'b list, 'c, ISO.A.state, C.constr, ConstrGen.kind, ISO.A.location) Test.t -> MachineDefTypes.model_params -> MachineDefTypes.system_state 

      (* ...and to fetch the instructions thereof *)
      val fetch_instructions : ISO.A.program -> ISO.A.start_points -> MachineDefTypes.system_state  -> MachineDefTypes.system_state 

      (* 
       * The main function, to calculate and display final states
       * Inputs:
       * m -- printing information
       * test -- information about the test from input file 
       *         (incl. name and constraints to check)
       * st -- initial state after fetching all instructions
       *)
      val calc_finals :
        Globals.ppmode ->
        ('a, 'b, 'c, 'd, C.constr, ConstrGen.kind, ISO.A.location) Test.t ->
        MachineDefTypes.system_state -> unit
    end
