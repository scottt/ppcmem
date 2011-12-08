(*********************************************************************)
(*                          Litmus                                   *)
(*                                                                   *)
(*        Luc Maranget, INRIA Paris-Rocquencourt, France.            *)
(*        Susmit Sarkar, University of Cambridge, UK.                *)
(*                                                                   *)
(*  Copyright 2010 Institut National de Recherche en Informatique et *)
(*  en Automatique and the authors. All rights reserved.             *)
(*  This file is distributed  under the terms of the Lesser GNU      *)
(*  General Public License.                                          *)
(*********************************************************************)


module type S = sig
  module A : Arch.S

  type prop = (A.location,A.V.v) ConstrGen.prop
  type constr = prop ConstrGen.constr

(* Does loc appears in constr ? *)
  val locations : constr -> A.location list
end

open Misc
open ConstrGen

module Make(A : Arch.S) : S with module A = A  = 
  struct
    module A = A
    module V = A.V

    type prop = (A.location,V.v) ConstrGen.prop
    type constr = prop ConstrGen.constr

    module LocSet =
      MySet.Make
        (struct
          type t = A.location
          let compare = A.location_compare
        end)

    let locations c =
      let locs =
        fold_constr
          (fun (loc,_) -> LocSet.add loc)
          c LocSet.empty in
      LocSet.elements locs
  end
