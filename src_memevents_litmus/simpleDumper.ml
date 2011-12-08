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

(*****************)
(* Parsable dump *)
(*****************)
module type I = sig
  module A : ArchBase.S

  type state
  val dump_state : out_channel -> state -> unit

  type constr
  val dump_constr : out_channel -> constr -> unit

  type location
  val dump_location : out_channel -> location -> unit
end

module Make(I:I) : sig
  val dump : out_channel ->
    Name.t ->
    (I.state, (int * I.A.pseudo list) list, I.constr, I.location)
        MiscParser.result
      -> unit
end = struct    
  open Printf
  open I

  let rec fmt_io io = match io with
  | A.Nop -> ""
  | A.Instruction ins -> A.dump_instruction ins
  | A.Label (lbl,io) -> lbl ^ ": " ^ fmt_io io
  | A.Macro (f,regs) ->
      sprintf
        "%s(%s)"
        f
        (String.concat "," (List.map A.pp_reg regs))

  let fmt_col (p,is) = sprintf "P%i" p::List.map fmt_io is

  let prog chan prog =
    let pp = List.map fmt_col prog in
    Misc.pp_prog chan pp
(*
    dump_procs chan prog ;
    iter_prog (dump_ios chan)
      (List.map snd prog)
*)
  open MiscParser

  let dump chan doc t =
    fprintf chan "%s %s\n"
      (Misc.pp_arch A.arch)
      doc.Name.name ;
    begin match doc.Name.doc with
    | "" -> ()
    | doc -> fprintf chan "\"%s\"\n" doc
    end ;
    fprintf chan "\n{%a}\n\n" dump_state  t.init ;
    prog chan t.prog ;
    fprintf chan "\n" ;
    begin match t.locations with
    | [] -> ()
    | locs ->
        fprintf chan "locations [" ;
        List.iter (fun (loc,t) -> match t with
        | MiscParser.I -> fprintf chan "%a; " I.dump_location loc
        | MiscParser.P -> fprintf chan "%a*; " I.dump_location loc)
          locs ;
        fprintf chan "]\n"
    end ;
    dump_constr chan t.condition ;
    output_char chan '\n'
end
