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

type ('prog,'nice_prog,'start,'state,'constr,'kind,'loc) t =
    {
     arch : Misc.arch ; 
     name : Name.t ;
     info : MiscParser.info ;
     program : 'prog ;
     nice_prog : 'nice_prog ;
     start_points : 'start ;
     init_state : 'state ;
     cond : 'constr ;
     quantifiers : (string * 'kind) list;
     flocs : 'loc list ;
   }

(* Name and nothing else *)
let simple_name test = test.name.Name.name

(* human-readable test name/filename combination *)
let readable_name test = 
  test.name.Name.humanname^ (if (Globals.get_our_runopts()).Globals.web_diagrams then "" else " \\iN{"^test.name.Name.name^"}")

(* and just the first part of that, for use in latex index *)
let very_readable_name test = 
  test.name.Name.humanname


module Make(A:Arch.S) =
  struct

    type result =
        (A.program, A.nice_prog, A.start_points,
         A.state, A.constr, A.quantifier,A.location) t

    open ConstrGen
    (*open Printf*)

(* Symb register allocation is external, since litmus needs it *)
   module Alloc = SymbReg.Make(A)
(* Code loader is external, since litmus tests need it too *)
    module Load = Loader.Make(A) 

    let build r_split t =
      let t = Alloc.allocate_regs t in
      let
          {MiscParser.init = init ;
           info = info ;
           prog = nice_prog ;
           condition = final ; 
	   kinds = quantifiers;
           locations = locs ;
	 } = t in

      let prog,starts = Load.load nice_prog in
      {
       arch = r_split.Splitter.arch ;
       name = r_split.Splitter.name ;
       info = info ;
       program = prog ;
       nice_prog = nice_prog ;
       start_points = starts ;
       init_state = A.build_state init ;
       cond = final ;
       quantifiers = quantifiers;
       flocs = List.map fst locs ;
     }


    let find_our_constraint test = 
      let bare = test.cond in
      let quantifier = 
      try
	List.assoc (Globals.get_our_runname ()) test.quantifiers 
      with
	Not_found -> 	
	  try 
	    List.assoc "default" test.quantifiers 
	  with 
	    Not_found -> (* None Specified *) ConstrGen.Unconstrained 
      in 
      match bare,quantifier with
      | ConstrGen.QueryState p,ConstrGen.Require -> ConstrGen.ForallStates p
      | ConstrGen.QueryState p,ConstrGen.Allow -> ConstrGen.ExistsState p
      | ConstrGen.QueryState p,ConstrGen.Forbid -> ConstrGen.NotExistsState p
      | _,_ -> bare



end
