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

(**********)
(* Digest *)
(**********)

module Make(A:ArchBase.S) :
    sig
      type init = MiscParser.state
      type prog = (int * A.pseudo list) list
      type locations = MiscParser.LocSet.t

      val refresh_labels : string -> prog -> prog
      val digest : init -> prog -> locations -> string
    end
    = struct

      open Printf

      type init = (MiscParser.location * SymbConstant.v) list
      type prog = (int * A.pseudo list) list
      type locations =  MiscParser.LocSet.t


      open MiscParser

(* Digest of initial state *)     
      let digest_init init =
        let init =
          List.sort
            (fun (loc1,v1) (loc2,v2) -> match location_compare loc1 loc2 with
            | 0 ->
                if SymbConstant.compare v1 v2 <> 0 then begin
                  Warn.fatal
                    "Location %s non-unique in init state"
                    (dump_location loc1)
                end ;
                0
            | c -> c)
            init in
        let init =
          Misc.rem_dups
            (fun (loc1,_) (loc2,_) -> location_compare loc1 loc2 = 0)
            init in
        Digest.string
          (String.concat "; "
             (List.map
                (fun (loc,v) -> sprintf "%s=%s"
                    (dump_location loc) (SymbConstant.pp v))
                init))

(* Code digest *)
      module StringSet = MySet.Make(String)

      let all_labels =
        List.fold_left 
          (fun k (_,code) ->
            List.fold_left 
              (A.fold_labels (fun k lbl -> StringSet.add lbl k))
              k code)
          StringSet.empty
          
      let change_labels f =
        List.map
          (fun (p,code) ->
            let code = List.map (A.map_labels f) code in
            p,code)

      module StringMap = Map.Make(String)

      let refresh_labels pref prog =
        let lbls = all_labels prog in
        let next = ref 0 in
        let env =
          StringSet.fold
            (fun lbl env ->
              let new_lbl = sprintf "L%s%02i" pref !next in
              incr next ;
              StringMap.add lbl new_lbl env)
            lbls StringMap.empty in
        change_labels
          (fun lbl ->
            try StringMap.find lbl env
            with Not_found -> assert false)
          prog

      let norm_labels = refresh_labels "" 

      let norm_instructions =
        List.map
          (fun (p,code) ->
            let code = List.map (A.pseudo_map A.norm_ins) code in
            p,code)

      let dump_pseudo =
        let rec dump_rec p k = match p with
        | A.Nop -> k
        | A.Instruction i -> A.dump_instruction i::k
        | A.Label (lbl,p) -> sprintf "%s:" lbl::dump_rec p k
        | A.Macro _ -> assert false (* applied after macro expansion *) in
        fun (_,ps) ->
          List.fold_right dump_rec ps []



      let digest_code code =
        (* Because I have introduced 64bits instructions,
           which we can remap to 32bits ones... *)
        let code = norm_instructions code in
        (* Because labels may change, when generated by macro
           expansion *)
        let code = norm_labels code in
        (* Just pretty_print code in a normalized way *)
        let code = List.map dump_pseudo code in
        let code =  Misc.string_of_prog code in
        Digest.string code


(* Observed locations digest *)
      let digest_observed locs =
        let locs = MiscParser.LocSet.elements locs in
        let locs = String.concat "; " (List.map dump_location locs) in
        Digest.string locs

        
      let digest init code observed =
        Digest.to_hex
          (Digest.string
             (digest_init init ^ digest_code code ^
              digest_observed observed))
    end
