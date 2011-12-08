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

open MachineDefUtils
open MachineDefValue
open MachineDefTypes
open Types
open Model_aux
module Html = Dom_html
let js = Js.string
(*open Printf*)

let js = js
let document = Html.window##document

let system_state = Html.createDiv document 
let litmus_test = Html.createTextarea document 
let transitions = Html.createDiv document 
let warnings = Html.createDiv document 
let options = Html.createDiv document
	
	

let next = Html.createInput ~_type:(js"button") document 
let undo = Html.createInput ~_type:(js"button") document 
let auto = Html.createInput ~_type:(js"button") document 
let reset = Html.createInput ~_type:(js"button") document 
let help = Html.createInput ~_type:(js"button") document 
let interactive = Html.createInput ~_type:(js"button") document 
let non_interactive = Html.createInput ~_type:(js"button") document 
let test_select = Html.createInput ~_type:(js"button") document
let start = Html.createInput ~_type:(js"button") document 
let select_options = Html.createInput ~_type:(js"button") document 


let _ = 

litmus_test##rows <- 15; litmus_test##cols <- 40;
litmus_test##style##border <- js "1px black none";
litmus_test##style##padding <- js "15px";
litmus_test##id <- js "litmus_test";
litmus_test##setAttribute((js "spellcheck"),(js "false"));
(*
system_state##rows <- 35; system_state##cols <- 100;*)
system_state##style##border <- js "1px black dashed";
system_state##style##padding <- js "15px";
system_state##id <- js "system_state";



(*transitions##rows <- 10; transitions##cols <- 100;*)
transitions##style##border <- js "1px black";
transitions##style##padding <- js "15px";
transitions##id <- js "transitions";


(*warnings##rows <- 10; warnings##cols <- 100;*)
warnings##style##border <- js "1px black";
(*warnings##style##padding <- js "15px";*)
warnings##style##color <- js "red";
warnings##id <- js "warnings";

undo##value <- js "Undo";
auto##value <- js "Auto";
reset##value <- js "Reset";
help##value <- js "Help";
next##value <- js "Next";
select_options##value <- js "Select Options";
interactive##value <- js "Interactive";
non_interactive##value <- js "Non Interactive";
test_select##value <- js "Select Test" ;
test_select##title <- js "Click to select tests from library";
interactive##id <- js "interact";
test_select##id <- js "test_select";
non_interactive##id <- js "n_interact";
undo##id <- js "undo";
auto##id <- js "auto";
reset##id <- js "reset";
help##id <- js "help";



type allowed_reads = (address * value) list

(* The info structure of state of search procedure *)
type info = {
    mutable trans_todo : int list;     (* follow list as numbered choices *)
    mutable choices_so_far : int list; (* numbered choices made so far *)
    mutable trans_so_far : trans list; (* transitions made so far *)
    mutable last_system_state : system_state option; (* the preceding system state (one step up in search tree) *)
    mutable allowed_acts : allowed_reads; (* constraints on read values *)
  }

type interactive_input = 
  | I_Choice of info * int
  | I_Auto
  | I_Undo
  | I_Quit

(* The state interaction manipulates *)
type interaction_state = {
    mutable cands : trans set; (* The set of transitions *)
    mutable info  : info;(* The info structure *)
  }

let ref_inp = ref {info = {trans_todo= !Globals.follow; choices_so_far=[]; trans_so_far = []; last_system_state=None; allowed_acts = []};cands = (Pset.empty compare)}
let ref_k_info = ref (fun (x:interaction_state) -> ())
let ref_numbered_cands = ref []
let ref_nmax = ref 0
let ref_k_undo = ref (fun () -> ())
let ref_k_userquit = ref (fun () -> ())
let ref_trans = ref ""
let _ = undo##onclick <- Dom_html.handler (fun _ -> ref_trans := "undo" ; next##click() ; Js._true)
let _ = auto##onclick <- Dom_html.handler (fun _ -> ref_trans := "auto" ; next##click() ; Js._true)

let display result warning append= 
	if append then
		system_state##innerHTML <- Js.string ((Js.to_string system_state##innerHTML) ^ result)
	else 
		begin
		system_state##innerHTML <- Js.string result;
		warnings##innerHTML <- Js.string warning;
		transitions##innerHTML <- Js.string "";
		auto##style##display <- Js.string "none";
		undo##style##display <- Js.string "none";
		reset##style##display <- Js.string "inline";
		help##style##display <- Js.string "inline"
		end


let radio value txt name checked =
  let b =
    Dom_html.createInput
      ~name:(js name) ~_type:(js "radio") Dom_html.document in
  b##checked <- Js.bool checked;
  b##value <- js (string_of_int value);
  b##onclick <-
    Dom_html.handler (fun _ -> ref_trans := Js.to_string b##value;next##click (); Js._true);
  let lab = Dom_html.createLabel Dom_html.document in
  Dom.appendChild lab b;
  Dom.appendChild lab (Dom_html.document##createTextNode (txt));
  lab


let replace_text text p r = 
	let x = jsnew Js.regExp_withFlags (js p,js "mg") in
	let y = js r in	
	text##replace(x,y)  

(* Take interaction data structure as input:
      cands : candidates enabled
      info  : previous info
   Produce interaction data structure (option) as output:
      cands : candidates enabled by user interaction
      info  : updated info
   and feed that to continuation
   OR undo
   OR quit *)
let interact_with_user : Globals.ppmode -> system_state -> interaction_state -> (interaction_state -> unit) -> (unit -> unit) -> (unit -> unit) -> unit =
  fun m s inp k_info k_undo k_userquit ->
          begin
            (*Printf.printf "\027[;H\027[J";  (* cursor to top left and clear *)
            Printf.printf "----------------------------------------------------------------------------\n";*)
            let nmax,numbered_cands = 
              let rec number n xs = match xs with [] -> n,[] | x::xs -> let nmax,ys = number (n+1) xs in nmax, (n,x)::ys in
              number 0 (List.sort compare_trans (Pset.elements inp.cands)) in

        let ui_state = Pp.make_ui_system_state_opt (inp.info.last_system_state) s numbered_cands in 
            (*Printf.printf "%s\n" (Pp.pp_ui_system_state m ui_state);*)
			let temp = js (Pp.pp_ui_system_state m ui_state) in
			(*Dom_html.window##alert  (replace_text temp "\\n" "</br>"); *)
			let text = replace_text 
							(replace_text (replace_text (replace_text temp "\\n" "<br/>") "\s" "&nbsp;") "<font" "<font ") "<span" "<span class='trans' " in 
				system_state##innerHTML <- text;
			if List.length numbered_cands = 0 then
	      begin
		(*Printf.printf "No enabled transitions\n";*)
			transitions##innerHTML <- js "<font color='red'><b>No enabled transitions</b></font>";
		k_info {info=inp.info;cands=inp.cands}
	      end
	    else
	      begin
		(*Printf.printf "Enabled transitions:\n" ;*)
		transitions##innerHTML <- js "";
		warnings##innerHTML <- js "";
		List.iter (fun (n,c) -> 
			(*	let output = Printf.sprintf "  %2s: %s \n" (Pp.colour_tran_id m (Printf.sprintf "%2d" n)) (Pp.pp_trans c) in*)
				let txt =  Pp.pp_trans m c in
					Dom.appendChild transitions (radio n (js txt) "trans" false);
					Dom.appendChild transitions (Html.createBr document)
(*					transitions##innerHTML <- js ((Js.to_string transitions##innerHTML )^"</br>"^output );*)
					
						  
		  (*Printf.printf "  %2s: %s \n" (Pp.colour_tran_id m (*Pp.pp_tran_id m*) (*({Globals.pp_kind=Globals.Ascii;Globals.pp_colours = (Globals.get_our_runopts()).Globals.colours })*) (sprintf "%2d" n)) (Pp.pp_trans m c)*)) numbered_cands;
		(*Printf.printf "----------------------------------------------------------------------------\n";*)
		
		ref_numbered_cands := numbered_cands;
		ref_nmax := nmax;
		ref_inp := inp;
		ref_k_info := k_info;
		ref_k_undo := k_undo;
		ref_k_userquit := k_userquit;
		
	end
end

let n_input s info cands nmax numbered_cands k_info k_undo k_userquit = 
	ref_trans := "";
	match info.trans_todo with
	| n::ns ->
    (*Printf.printf "%d: %s (from follow list)\n" n (Pp.pp_trans (List.assoc n numbered_cands));*) 
    if n < 0 || n >= nmax then 
		begin
			warnings##innerHTML <- js (Printf.sprintf "follow specification error: %d must be an integer in [0,%d]\n<shifting to interactive mode>\n" n (nmax-1)); 
			!(ref_inp).info.trans_todo <- []
		end
    else 
		begin
			k_info { info = {{info with choices_so_far = info.choices_so_far @ [n]} with trans_todo = ns};
		              cands = Pset.singleton (trans_compare) (List.assoc n numbered_cands)}
		end		
	 | [] -> 
		if String.compare s "auto" = 0 then 
	      begin
		    Globals.set_interactive false;
			k_info {info = info;cands = cands} 
		  end
		else if List.mem s ["u";"undo"] then 
		  match info.choices_so_far with
		  | [] -> warnings##innerHTML <- js "cannot undo from the initial state\n"
		  | _ ->  k_undo ()
		else if List.mem s ["q";"quit"] then 
			k_userquit ()
		else
			let n = try int_of_string s with _ -> warnings##innerHTML <- js "Invalid input! Try again"; -1 in
			if n <0 || n>= nmax then 
				warnings##innerHTML <- js (Printf.sprintf "syntax error: must be an integer in [0,%d], \"undo\",\"u\", \"quit\", \"q\", or \"auto\"\n" (nmax-1))
			else 
				k_info { info = {info with choices_so_far = info.choices_so_far @ [n]}; 
					     cands = (Pset.singleton (trans_compare) (List.assoc n numbered_cands))}

			
let _ = next##onclick <- Html.handler (fun _ -> (n_input !ref_trans !(ref_inp).info !(ref_inp).cands !ref_nmax !ref_numbered_cands !(ref_k_info) !(ref_k_undo) !(ref_k_userquit));Js._false )
	

let ask_quit_or_undo k_undo k_userquit =()
 (* Printf.printf "----------------------------------------------------------------------------\n";
  let rec inp () = 
    let () = Printf.printf "Undo last transition, to continue exploration, or quit [u/q]: " in
    let s = try read_line () with End_of_file -> Printf.printf "\n"; "quit" in
    if s = "u" || s = "undo" then k_undo ()
    else if s = "q" || s = "quit" then k_userquit ()
    else (Printf.printf "syntax error, please input u/q/undo/quit\n"; inp ()) in
  inp ()*)

