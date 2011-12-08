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

open MiscParser
open Printf

module type S = sig
  type v
  type location
  type pseudo

  type ('loc,'v) t = ('loc,'v, pseudo) MiscParser.r3
      
  val allocate_regs :
      (MiscParser.location, MiscParser.maybev) t -> (location,v) t
end

module Make (A:Arch.S) : S 
with type v = A.V.v and type location = A.location
and type pseudo = A.pseudo
 = struct

   type v = A.V.v
   type location = A.location
   type pseudo = A.pseudo
   type ('loc,'v) t = ('loc,'v, pseudo) MiscParser.r3
      
(******************************************************)
(* All those to substitute symbolic regs by real ones *)
(******************************************************)

  let get_reg name = match A.parse_reg name with
  | Some r -> r
  | None -> Warn.user_error "%s is not a register" name

  let finish_reg = get_reg

  let finish_location f_reg loc = match loc with
  | Location_global m -> A.maybev_to_location m
  | Location_reg (i,r) -> A.Location_reg (i,finish_reg r)
  | Location_sreg reg  ->
      let p,r = f_reg reg in A.Location_reg (p,r)

  let finish_atom f_reg (loc,v) =
    finish_location f_reg loc, A.V.maybevToV v

  let finish_state f_reg = List.map (finish_atom f_reg)

  let finish_locations f_reg =
    List.map (fun (loc,t) -> finish_location f_reg loc,t)

  let finish_constr f_reg = ConstrGen.map_constr (finish_atom f_reg)

  let rec finish_pseudo f_reg =
    A.pseudo_map (A.map_regs (fun r -> r) f_reg)

  let finish_code f_reg = List.map (finish_pseudo f_reg)


(**********************************)	
(* All those to collect registers *)
(**********************************)	

  module StringSet =
    MySet.Make
      (struct
	type t = string
	let compare = String.compare
      end)

  module ProcRegSet = 
    MySet.Make
      (struct
	type t = int * A.reg
	let compare (p1,r1) (p2,r2) = match compare p1 p2 with
	| 0 -> A.reg_compare r1 r2
	| r -> r
      end)

  module RegSet =
    MySet.Make
      (struct
	type t = A.reg
	let compare = A.reg_compare
      end)


  let rec collect_pseudo f =
    A.pseudo_fold
      (fun k ins -> A.fold_regs f k ins)

  let collect_prog =
    List.map
      (List.fold_left
	 (collect_pseudo  (RegSet.add,StringSet.add))
	 (RegSet.empty,StringSet.empty))
      

  let collect_location loc (regs,symbs as c) = match loc with
  | Location_reg (p,r) ->
      ProcRegSet.add (p,get_reg r) regs,symbs
  | Location_sreg reg ->
      regs,StringSet.add reg symbs
  | Location_global _ -> c

  let collect_atom (loc,(_:maybev)) = collect_location loc

  let collect_state = List.fold_right collect_atom

  let collect_constr = ConstrGen.fold_constr collect_atom

  let collect_locs = List.fold_right (fun (loc,_) -> collect_location loc)

(*********************************************)
(* Here we go: collect, allocate, substitute *)
(*********************************************)
  open MiscParser

  let pp_reg_set chan rs =
    RegSet.pp chan "," (fun chan r -> fprintf chan "%s" (A.pp_reg r)) rs
  and pp_string_set chan s =
      StringSet.pp chan "," (fun chan r -> fprintf chan "%s" r) s

  let allocate_regs test =
    let initial = test.init
    and prog = test.prog
    and final = test.condition
    and locs = test.locations in
    (* Collect all registers, either real or symbolic *)
    let regs,symbs =
      collect_constr final
        (collect_locs locs
	   (collect_state initial
	      (ProcRegSet.empty,StringSet.empty)))
    in

    let in_code = collect_prog (List.map snd prog) in
    (* Control register usage, ambiguity is possible,
       for unconstrained symbolic regs *)
    let (_,bad) =
      List.fold_left
	(fun (seen,bad) (_,symbs_p) ->
	  let symbs_p = StringSet.inter symbs_p symbs in
	  let bad =
	    StringSet.union
	      bad (StringSet.inter seen symbs_p) in
	  let seen = StringSet.union symbs_p seen in
	  seen,bad)
	(StringSet.empty,StringSet.empty)
	in_code in
    if not (StringSet.is_empty bad) then begin
      let msg =
	sprintf "ambiguous symbolic register(s): {%s}"
	  (String.concat ","
	     (StringSet.elements bad)) in
      Warn.user_error "%s" msg
    end ;
    (* Perform allocation of symbolic registers to real ones *)
    let envs =
      List.map2
	(fun (p,_) (regs_p,symbs_p) ->
	  let regs_cstr =
	    ProcRegSet.fold
	      (fun (q,reg) k ->
		if p=q then RegSet.add reg k else k)
	      regs RegSet.empty in
	  let free_regs =
	    RegSet.diff (RegSet.of_list A.allowed_for_symb)
	      (RegSet.union regs_p regs_cstr) in
	  let env,_ =
	    StringSet.fold
	      (fun name (env,free_regs) -> match free_regs with
	      | [] ->
		  Warn.user_error
		    "not enough registers for all those symbolic registers"
	      | next::free_regs ->
		  (name,next)::env,free_regs)
	      symbs_p ([],RegSet.elements free_regs) in
	  p,env)
	prog in_code in
    (* Replace symbolic registers *)
    let prog =
      List.map2
	(fun (proc,code) (_,env) ->
	  let replace name =
	    try List.assoc name env
	    with Not_found -> assert false in
	  proc,finish_code replace code)
	prog envs in
    
    let env =
      List.fold_left
	(fun k (p,env_p) ->
	  List.fold_left
	    (fun k (symb,reg) -> (symb,(p,reg))::k)
	    k env_p)
	[] envs in
    let replace name =
      try List.assoc name env
      with Not_found ->
	Warn.user_error
	  "symbolic register %%%s does not appear in code" name in
    { test with
      init = finish_state replace initial ;
      prog = prog;
      condition = finish_constr replace final;
      locations = finish_locations replace locs;
    }

end
