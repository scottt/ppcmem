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
open MachineDefTypes
open MachineDefInstructionSemantics

type source = Inchannel of in_channel| String of string

let pp_version () =
  Printf.printf "%s" Machine_version.machine_version;
  Printf.printf "%s" Version.code_version

(* do_stuff reads in a test from an input channel, parses it, 
   constructs the initial state, and runs the model with calc_finals *)
module Make_do_stuff
    (ISO:Iso.S)
    (L:GenParser.LexParse with type instruction = ISO.A.pseudo)
    =
  struct
    module A = ISO.A
    module P = GenParser.Make(A)(L)
    module T = Test.Make(A)
    module R = Run.Make(ISO)

    let do_stuff (in_chan:source) name test_splitted = 
      let parsed =(* P.parse in_chan test_splitted in*)
        (match in_chan with
        | Inchannel x -> P.parse x test_splitted
        | String x -> P.parse_web x test_splitted
		) in
      let test = T.build test_splitted parsed in
      if !Globals.debug >= 0 then
        begin
          pp_version () ;
          Printf.printf "Trying %s\n" (Test.readable_name test);
        end;
      if !Globals.debug > 1 then R.pp_test_debug test;
      let initial_state = R.initial_state_of_test test (Globals.get_our_runopts()).Globals.model_params in
      let initial_state_fetched = R.fetch_instructions test.Test.program test.Test.start_points initial_state in 
      let () = R.calc_finals (Globals.get_our_ppmode ()) test initial_state_fetched
      in
      ()
  end

(* instantiate Make_do_stuff to PPC *)
module PPC = PPCArch.Make(SymbValue) 
module PPCISO = PPCIso.Make(SymbValue) 
module PPCLexParse = struct
  type instruction = PPC.pseudo
  type token = PPCParser.token

  let lexer = PPCLexer.token
  let parser_ = PPCParser.main
end 
module Made_do_stuff_PPC = Make_do_stuff(PPCISO)(PPCLexParse)

(* instantiate Make_do_stuff to ARM *)
module ARM = ARMArch.Make(SymbValue) 
module ARMISO = ARMIso.Make(SymbValue) 
module ARMLexParse = struct
  type instruction = ARM.pseudo
  type token = ARMParser.token

  let lexer = ARMLexer.token
  let parser_ = ARMParser.main
end 
module Made_do_stuff_ARM = Make_do_stuff(ARMISO)(ARMLexParse)


let from_chan name in_chan out_chan =  
(* First split the input file in sections *)
  let { Splitter.arch=arch } as test_splitted =  Splitter.split_channel name in_chan in
(* Then call appropriate do_stuff, depending upon the arch of the test *)
  match arch with
  | Misc.PPC ->
      let _ = Globals.set_pparch Globals.PP_PPC in
      Made_do_stuff_PPC.do_stuff (Inchannel in_chan) name test_splitted
  | Misc.X86 -> Warn.fatal "Cannot handle X86"
  | Misc.ARM ->  (* Warn.fatal "Cannot really handle ARM"; *)
      let _ = Globals.set_pparch Globals.PP_ARM in
      Made_do_stuff_ARM.do_stuff (Inchannel in_chan) name test_splitted

let from_web name in_chan =
(* First split the input file in sections *)
  let { Splitter.arch=arch } as test_splitted =  Splitter.split_string name in_chan in
(* Then call appropriate do_stuff, depending upon the arch of the test *)
  match arch with
  | Misc.PPC ->
      let _ = Globals.set_pparch Globals.PP_PPC in
      Made_do_stuff_PPC.do_stuff (String in_chan) name test_splitted
  | Misc.X86 -> (*Warn.fatal "Cannot handle X86"*)()
  | Misc.ARM -> (* Warn.fatal "Cannot handle ARM"*)
      let _ = Globals.set_pparch Globals.PP_ARM in
      Made_do_stuff_ARM.do_stuff (String in_chan) name test_splitted

let from_file name out_chan =
  Misc.input_protect
    (fun in_chan ->
      from_chan name in_chan out_chan) name

let from_files names =
  let out_chan =
    stdout in
  let _ =
    List.fold_left
      (fun () name ->
        try from_file name out_chan
        with Misc.UserError s -> eprintf "Error in test %s:%s\n" name s)
      ()  names in
  begin
    flush out_chan ;
    ()
  end


(* *********************** *)
(* command-line processing *)
(* *********************** *)

let model_strings = ["partial_cc";"late_cc";
	             "avoid_deadlock";"no_avoid_deadlock";
                     "coherence_points";"no_coherence_points";
	             "loose_thread";"tight_thread";
                     "lwsync_read_block"; "lwsync_read_restart";
                     "restart_forwarded"; "dont_restart_forwarded";
	           ]

let pp_model m =
  "[" ^
  (match m.ss.coherence_commit with
  | Partial_CC -> "partial_cc"
  | Late_CC -> "late_cc") ^
  "; " ^
  (match m.ss.pcc_deadlock_avoid with
  | true -> "avoid_deadlock"
  | false -> "no_avoid_deadlock") ^
  "; " ^
  (match m.ss.coherence_points with
  | true -> "coherence_points"
  | false -> "no_coherence_points") ^
  "; " ^
  (match m.t.thread_loose_tight with
  | Thread_loose -> "loose_thread"
  | Thread_tight -> "tight_thread") ^
  "; " ^
  (match m.t.thread_lwsync with
  | Lwsync_read_restart -> "lwsync_read_restart"
  | Lwsync_read_block -> "lwsync_read_block") ^
  "; " ^
  (match m.t.thread_restart_forwarded with
  | Restart_forwarded_reads -> "restart_forwarded"
  | Dont_restart_forwarded_reads -> "dont_restart_forwarded") ^
  "]"


let parse_and_update_model s mbef =
  match s with
  | "partial_cc" -> {mbef with ss = {mbef.ss with coherence_commit = Partial_CC}}
  | "late_cc" -> {mbef with ss = {mbef.ss with coherence_commit = Late_CC}}
  | "avoid_deadlock" -> {mbef with ss = {mbef.ss with pcc_deadlock_avoid = true}}
  | "no_avoid_deadlock" -> {mbef with ss = {mbef.ss with pcc_deadlock_avoid = false}}
  | "coherence_points" -> {mbef with ss = {mbef.ss with coherence_points = true}}
  | "no_coherence_points" -> {mbef with ss = {mbef.ss with coherence_points = false}}
  | "loose_thread" -> {mbef with t = {mbef.t with thread_loose_tight = Thread_loose}}
  | "tight_thread" -> {mbef with t = {mbef.t with thread_loose_tight = Thread_tight}}
  | "lwsync_read_restart" -> {mbef with t = {mbef.t with thread_lwsync = Lwsync_read_restart}}
  | "lwsync_read_block" -> {mbef with t = {mbef.t with thread_lwsync = Lwsync_read_block}}
  | "restart_forwarded" -> {mbef with t = {mbef.t with thread_restart_forwarded = Restart_forwarded_reads}}
  | "dont_restart_forwarded" -> {mbef with t = {mbef.t with thread_restart_forwarded = Dont_restart_forwarded_reads}}
  | _ -> Warn.user_error "Unknown model %s" s

