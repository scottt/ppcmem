open Printf
open Top

let pp_cmdline () =
  Printf.printf "#Command line: %s\n" 
    (String.concat " " (Array.to_list Sys.argv))       


let quiet = ref false

let opts =  Arg.align
    [("-version",Arg.Unit (fun () -> pp_version () ; exit 0),
      " show version information and exit");
     ("-v",Arg.Unit (fun b -> Globals.debug := !Globals.debug + 1), " verbose debug output (repeatable)");
     ("-q",Arg.Set quiet," be quiet");
     ("-auto", Arg.Unit
        (fun () -> quiet := true ; Globals.auto := true),
      " produce output suitable for the dont tool");
     ("-O",Arg.Unit (fun () ->  Globals.set_fast true)," enable (experimental) optmisations aiming at speed");
     ("-debug",Arg.Unit (fun b -> Globals.debug := 10), " as above, highest level of debug output");
     ("-interactive",Arg.Bool (fun b -> Globals.set_interactive b; Globals.set_colours true), "<bool> interactive search");
     ("-colours",Arg.Bool (fun b -> Globals.set_colours b), "<bool> colours in interactive terminal output");
     ("-ppkind",Arg.String (fun s -> Globals.set_ppkind s), "<Ascii|Latex|Html> ppkind");
     ("-follow",Arg.String (fun s -> Globals.follow := List.map int_of_string (Str.split (Str.regexp ";") s)), "<int-list> list of transition candidates to follow, semicolon-separated");
     ("-model",Arg.String (fun s -> Globals.set_model (parse_and_update_model s ((Globals.get_our_runopts()).Globals.model_params))),"<model> model to check (choices: "^String.concat "; " model_strings^"), currently:"^pp_model ((Globals.get_our_runopts()).Globals.model_params));
     ("-safe", Arg.Unit (fun () -> Globals.set_safemode true)," explore all paths, exhaustively (default)");
     ("-quick", Arg.Unit (fun ()-> Globals.set_safemode false), " quick exploration of possibilities (experimental: enumerates all conceivable rfmaps, doing a search for each, but pruning branches that violate that rfmap) (opposite of -safe)");
     ("-onlystate", Arg.Unit (fun () -> Globals.set_statematchmode true), " check only whether the final state mentioned explicitly in the test is observable (experimental)");
     ("-allstates", Arg.Unit (fun () -> Globals.set_statematchmode false), " explore all reachable states (default)");
   ]

let usage = sprintf   "Usage: ppcmem [options]* filename\n       ppcmem -help   to show options"

let sources = ref []

let () = Arg.parse opts (fun s -> sources := s :: !sources) usage 

let () = 
  match ((Globals.get_our_runopts ()).Globals.statematchmode),((Globals.get_our_runopts ()).Globals.safemode) with 
  | (true,false) -> 
      eprintf "Fatal error: %s\n%!" "cannot combine -quick and -onlystate" ;
      exit 2
  | (_,_) -> ()

let () = match !sources with [] -> eprintf "%s\n" usage; exit 2 | _ -> ()

let () = if !quiet then  Globals.debug := -1

let sources = Misc.expand_argv !sources

let () =
  if !Globals.debug < 0 && not !Globals.auto then begin 
    pp_cmdline () ;
    printf "#Model: %s\n" (pp_model ((Globals.get_our_runopts()).Globals.model_params))
  end ;
  try
	Pp.linebreak_init();
    from_files sources ; exit 0
  with 
  | Misc.Fatal msg ->
      eprintf "Fatal error: %s\n%!" msg ;
      exit 2
  | Globals.Interactive_quit ->
      exit 0
