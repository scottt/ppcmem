
open Top
open Interact

module Html = Dom_html
let js = Js.string
let document = Html.window##document

let button name callback =
  let input = Html.createInput ~_type:(js"submit") document in
  input##value <- js name;
  input##onclick <- Html.handler callback;
  input

let _ = Globals.set_ppkind "Html";
		Globals.debug := -1;
		Globals.set_colours true;
		Pp.linebreak_init()


let evaluate s =
	from_web "litmus test" s


let radio value txt name checked style=
  let b =
    Dom_html.createInput
      ~name:(js name) ~_type:(js "radio") Dom_html.document in
  b##checked <- Js.bool checked;
  b##value <- js value;
  b##onclick <-
	Dom_html.handler (fun _ -> (*match b##checked with
								| True -> *)Globals.set_model (parse_and_update_model value ((Globals.get_our_runopts()).Globals.model_params)); Js._true);

	let p = Html.createDiv document in 
	let lab = Dom_html.createLabel Dom_html.document in
	Dom.appendChild lab b;
	p##className <- js style;
	Dom.appendChild lab (Dom_html.document##createTextNode ( js txt));
	Dom.appendChild p lab; 
	p


let checkbox value txt name checked style=
  let b =
    Dom_html.createInput
      ~name:(js name) ~_type:(js "checkbox") Dom_html.document in
  b##checked <- Js.bool checked;
  b##value <- js value;
  b##onclick <-
    Dom_html.handler (fun _ -> (*Dom_html.window##alert (js (string_of_bool (Js.to_bool b##checked)));*)
					match Js.to_bool b##checked with
					| true -> Globals.set_model (parse_and_update_model value ((Globals.get_our_runopts()).Globals.model_params)); Js._true;
					| false -> Globals.set_model (parse_and_update_model ("no_"^value) ((Globals.get_our_runopts()).Globals.model_params)); Js._true;
					);
  let p = Html.createDiv document in 
  let lab = Dom_html.createLabel Dom_html.document in
  p##className <- js style;
  Dom.appendChild lab b;
  Dom.appendChild lab (Dom_html.document##createTextNode ( js txt));
  Dom.appendChild p lab; 
  p



let onload _ =
  let main =
    Js.Opt.get (document##getElementById(js"main"))
      (fun () -> assert false)
  in
  	let p = Html.createDiv document in 
	p##className <- js "textDiv";
    Dom.appendChild p Interact.litmus_test;
    Dom.appendChild main p;

	let fieldset = Html.createFieldset document in 
	let legend =  (Html.createLegend document) in 
		legend##innerHTML <- js "System State";
	Dom.appendChild fieldset legend;
	Dom.appendChild fieldset Interact.system_state;
    Dom.appendChild main fieldset; 
	fieldset##style##display <- js "none"; 

    Dom.appendChild main transitions;

    Dom.appendChild main (Html.createBr document);
	let model_options = Html.createDiv document in
	(*Dom.appendChild model_options (Dom_html.document##createTextNode ( js (pp_model ((Globals.get_our_runopts()).Globals.model_params))));*)
	Dom.appendChild main model_options;
	model_options##className <- js "textDiv";
	model_options##style##display <- js "none";
	
    Dom.appendChild main (Html.createBr document);
	Dom.appendChild main warnings;
	warnings##style##display <- js "none"; 
	options##style##display <- js "none"; 
	transitions##style##display <- js "none"; 



(*	let options = Html.createDiv document in*)

  	let heading = Html.createDiv document in 
	heading##className <- js "textDiv";

	Dom.appendChild heading (Dom_html.document##createTextNode ( js "Model Options (Expert Use Only)"));
    Dom.appendChild heading (Html.createBr document);
    Dom.appendChild heading (Html.createHr document);
	options##id <- js "options";
	options##className <- js "window";
	Dom.appendChild options heading;
	Dom.appendChild options (checkbox "avoid_deadlock" "Avoid Deadlock" "model_options" true "left");
	(*Dom.appendChild options (checkbox "no_avoid_deadlock" "no_avoid_deadlock" "model_options" false "4");*)
	Dom.appendChild options (checkbox "coherence_points" "Coherence_points" "model_options" true "right");
	(*Dom.appendChild options (checkbox "no_coherence_points" "no_coherence_points" "model_options" true "6");*)
    Dom.appendChild options (Html.createHr document);

	Dom.appendChild options (Dom_html.document##createTextNode ( js "Coherence Commit"));
    Dom.appendChild options (Html.createBr document);
	Dom.appendChild options (radio "partial_cc" "partial_cc" "coherence_commit" true "left");
	Dom.appendChild options (radio "late_cc" "late_cc" "coherence_commit" false "right");
    Dom.appendChild options (Html.createHr document);
	Dom.appendChild options (Dom_html.document##createTextNode ( js "Thread Loose Tight"));
    Dom.appendChild options (Html.createBr document);
	Dom.appendChild options (radio "loose_thread" "Loose Threads" "thread_loose_tight" true "left");
	Dom.appendChild options (radio "tight_thread" "Tight Threads" "thread_loose_tight" false "right");
    Dom.appendChild options (Html.createHr document);
	Dom.appendChild options (Dom_html.document##createTextNode ( js "Thread Lwsync"));
    Dom.appendChild options (Html.createBr document);
	Dom.appendChild options (radio "lwsync_read_restart" "Lwsync Read Restart" "thread_lwsync" false "left");
	Dom.appendChild options (radio "lwsync_read_block" "Lwsync Read Block" "thread_lwsync" true "right");
  	let footer = Html.createDiv document in 
	footer##className <- js "textDiv";
	let close = Html.createInput ~_type:(js"button") document in
	close##value <- js "Done";
	close##className <- js "close_options";
	Dom.appendChild footer close;
	Dom.appendChild options footer;
	
	Dom.appendChild main options;

    Dom.appendChild main next;
	next##style##display <- js "none";
    Dom.appendChild main (Html.createHr document);
	let p1 = Html.createDiv document in 
	p1##className <- js "textDiv";

    Dom.appendChild p1 interactive;
    Dom.appendChild p1 non_interactive;
	Dom.appendChild p1 test_select; 
	Dom.appendChild p1 select_options; 
	select_options##id <- js "select_options";

    Dom.appendChild p1 undo;
    Dom.appendChild p1 auto;
    Dom.appendChild p1 reset;
    Dom.appendChild p1 help;
	interactive##className <- js "button2";
	non_interactive##className <- js "button2";
	test_select##className <- js "button2";
	select_options##className <- js "button2";
	help##className <- js "button2";
	Dom.appendChild main p1;
	undo##style##display <- js "none";
	auto##style##display <- js "none";
	reset##style##display <- js "none";
	(*help##style##display <- js "none";*)
    Dom.appendChild main (Html.createBr document);
    let eval_button= (button "Evaluate"
     (fun _ ->
             evaluate (Js.to_string (litmus_test##value));
			litmus_test##style##display <- js "none"; 
			fieldset##style##display <- js "block"; 
			model_options##style##display <- js "block";
			model_options##innerHTML <- ( js ("Model Options : " ^ (pp_model ((Globals.get_our_runopts()).Globals.model_params))));
			(if ((Globals.get_our_runopts()).Globals.interactive) then 
			begin 
				undo##style##display <- js "inline";
				undo##className <- js "button1";
				auto##style##display <- js "inline"; 
				auto##className <- js "button1";
				reset##style##display <- js "inline"; 
				reset##className <- js "button1";
				help##style##display <- js "inline"; 
				help##className <- js "button1";
			end);
			warnings##style##display <- js "block"; 
			interactive##style##display <- js "none";
			non_interactive##style##display <- js "none";
			test_select##style##display <- js "none";
			select_options##style##display <- js "none";
      Js._false)) in
    Dom.appendChild main eval_button;
	eval_button##style##display <- js "none";
	interactive##onclick <- Html.handler (fun _ ->	Globals.set_interactive true ;
													warnings##innerHTML <- js "";
													eval_button##click ();Js._false);

	non_interactive##onclick <- Html.handler (fun _ ->  Globals.set_interactive false ;
														warnings##innerHTML <- js "";
														eval_button##click ();Js._false);

    Js._false

let _ = Html.window##onload <- Html.handler onload


