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


(***********************************************)
(* Extra functionalities for all architectures *)
(***********************************************)

(* Input signature, a reduced Arch.ARCH *)

module type I = sig
  module V : Value.S

  type arch_reg
  val pp_reg : arch_reg -> string
  val reg_compare : arch_reg -> arch_reg -> int

  type arch_instruction

end

(* Output signature, fonctionalities added *)
module type S = sig

  module I : I

  type global_loc = I.V.v 
 
  type proc = int
  val pp_proc : proc -> string

  type program_order_index = int
  val pp_prog_order_index : program_order_index -> string

  val zero_po_index : program_order_index
  val next_po_index : program_order_index -> program_order_index

  type ioid = int

  type inst_instance_id = {
      proc       : proc;
      program_order_index   : program_order_index;
      ioid : ioid;
      inst : I.arch_instruction; (* Just here for pretty printing *)
    }

  val inst_instance_compare :
      inst_instance_id -> inst_instance_id -> int

  include Location.S
  with type loc_reg = I.arch_reg and type loc_global = I.V.v

  val undetermined_vars_in_loc : location -> I.V.v option
  val simplify_vars_in_loc : I.V.solution ->  location -> location
  val map_loc : (I.V.v -> I.V.v) -> location -> location

  type state (* Now abstract, suicide ? *)

  val state_empty : state
  val pp_state : state -> string
  val dump_state : state -> string
  val pp_nice_state :
      state -> string (* delim, as in String.concat  *)
	-> (location -> I.V.v -> string) -> string

  (* for explict state construction *)
  val build_state : (location * I.V.v) list -> state
  val build_concrete_state : (location * int) list -> state


  (* No comment *)
  val state_is_empty : state -> bool
  val state_to_list : state -> (location * I.V.v) list
  val state_size : state -> int

  (* Look for what loc is bound to *)
  exception LocUndetermined (* raised when location is yet unkown *)
  val look_in_state : state -> location -> I.V.v

  (* Add a binding , shadow previous binding if any *)
  val state_add : state -> location -> I.V.v -> state

  (* Does loc binds v in state ? *)
  val state_mem : state -> location -> I.V.v -> bool

  (* State restriction to some locations *)
  val state_restrict : state -> (location -> bool) -> state

  (* Set of states *)
  module StateSet : MySet.S with type elt = state

  (*********************************)
  (* Components of test structures *)
  (*********************************)

  (* Test structures represent programmes loaded in memory
     and ready to start, plus some items that describe
     the test, such as its name (cf. Test.mli) *)


  (* Code memory is a mapping from labels to sequences of instructions,
     Too far from actual machine, maybe *)
  type code = (I.V.cst * I.arch_instruction) list

  module LabelMap : Map.S with type key = string


  (* Program loaded in memory *)
  type program = code LabelMap.t

  (* A starting address per proc *)
  type start_points = (proc * code) list


  (* Constraints *)
  type prop =  (location,I.V.v) ConstrGen.prop
  type constr = prop ConstrGen.constr
  type quantifier = ConstrGen.kind

  (* Old versions of previous types *)
  type inst_annot =
    | Precond of location * I.V.v
    | LitmusLabel of string

  type litmus_program = (I.V.v  * I.arch_instruction) list
  type run_skeleton =
      (proc * (I.V.v * inst_annot list) list) list


end

module Make(I:I) : S with module I = I
= struct

  module I = I

  type global_loc = I.V.v

  type proc = int

  let pp_proc = string_of_int

  type program_order_index = int
  let pp_prog_order_index = string_of_int

  let zero_po_index = 0
  let next_po_index po = po + 1

  type ioid = int

  type inst_instance_id = {
      proc       : proc;
      program_order_index   : program_order_index;
      ioid : ioid;
      inst : I.arch_instruction ;
    }


  let inst_instance_compare i1 i2 = match Misc.int_compare i1.proc i2.proc with
  | 0 -> Misc.int_compare i1.program_order_index i2.program_order_index
  | r -> r

  include Location.Make
      (struct
        type arch_reg = I.arch_reg
        let pp_reg = I.pp_reg
        let reg_compare = I.reg_compare

        type arch_global = I.V.v
        let maybev_to_global =  I.V.maybevToV
        let pp_global = I.V.pp_v
        let global_compare = I.V.compare
      end)

  let undetermined_vars_in_loc l =  match l with
  | Location_reg _ -> None
  | Location_local_buffer a
  | Location_global a ->
      if I.V.is_var_determined a then None
      else Some a


  let simplify_vars_in_loc soln l = match l with
  | Location_reg _  -> l
  | Location_local_buffer a ->
      Location_local_buffer (I.V.simplify_var soln a)
  | Location_global a ->
      Location_global (I.V.simplify_var soln a)

  let map_loc fv loc = match loc with
  | Location_reg _ -> loc
  | Location_local_buffer a -> Location_local_buffer (fv a)
  | Location_global a -> Location_global (fv a)

(***************************************************)
(* State operations, implemented with library maps *)
(***************************************************)
  module State =
    Map.Make
      (struct
	type t = location
        let compare = location_compare
      end)

  type state = I.V.v State.t

  let state_empty = State.empty

  let pp_nice_state st delim pp_bd =
    let bds =
      State.fold
	(fun l v k -> (pp_bd l v)::k)
	st [] in
    String.concat delim  (List.rev bds)

  let pp_state st =
    pp_nice_state st " "
      (fun l v -> pp_location l false ^ "\\mathord{=}" ^ I.V.pp_v v ^";")

  let dump_state st =
    pp_nice_state st " "
      (fun l v -> pp_location l false ^ "=" ^ I.V.pp_v v ^";")

  let build_state bds =
    List.fold_left (fun st (loc,v) -> State.add loc v st)
      State.empty bds

  let build_concrete_state bds =
    List.fold_left
      (fun st (loc,v) ->
	State.add loc (I.V.intToV v) st)
      State.empty bds

  let state_is_empty = State.is_empty

  let state_to_list st =
    List.rev (State.fold (fun l v k -> (l,v)::k) st [])

  let state_size st = State.fold (fun _ _ k -> 1+k) st 0

(* State sets *)

  module StateSet =
    MySet.Make
      (struct
	type t = state

	let compare st1 st2 = State.compare I.V.compare st1 st2
      end)


(* To get protection against wandering undetermined locations,
   all loads from state are by this function *)
  exception LocUndetermined

  let look_in_state st loc =
    match undetermined_vars_in_loc loc with
    | Some _ -> 
    (* if loc is not determined, then we cannot get its
       content yet *)
	raise LocUndetermined
    | None ->
	try State.find loc st with Not_found -> I.V.zero

  let state_add st l v = State.add l v st

  let forall_in_state st p =
    State.fold (fun l v k -> p l v && k) st true

  let state_mem st loc v =
    try
      let w = look_in_state st loc in
      I.V.compare v w = 0
    with LocUndetermined -> assert false


  let state_restrict st loc_ok =
    State.fold
      (fun loc v k ->
        if loc_ok loc then State.add loc v k
        else k)
      st State.empty

  (*********************************)
  (* Components of test structures *)
  (*********************************)

  (* Code memory is a mapping from globals locs, to instructions *)
  type code = (I.V.cst * I.arch_instruction) list

  module LabelMap =
    Map.Make
      (struct
	type t = string
	let compare = String.compare
      end)


  (* Programm loaded in memory *)
  type program = code LabelMap.t

  (* A starting address per proc *)
  type start_points = (proc * code) list


  (* Constraints *)
  type prop =  (location,I.V.v) ConstrGen.prop
  type constr = prop ConstrGen.constr
  type quantifier = ConstrGen.kind

  (* Old versions of previous types *)
  type inst_annot =
    | Precond of location * I.V.v
    | LitmusLabel of string

  type litmus_program = (I.V.v * I.arch_instruction) list
  type run_skeleton =
      (proc * (I.V.v * inst_annot list) list) list

end


