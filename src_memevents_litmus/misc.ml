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

exception UserError of string
exception Fatal of string
exception Exit of string

(*********)
(* Archs *)
(*********)

type arch =
  | X86
  | PPC
  | ARM

let parse_arch s = match s with
| "X86" -> Some X86
| "PPC" -> Some PPC
| "ARM" -> Some ARM
| _ -> None

let lex_arch s = match parse_arch s with
| Some a -> a
| None -> assert false


let pp_arch a = match a with
| X86 -> "X86"
| PPC -> "PPC"
| ARM -> "ARM"

(********************************)
(* ppmode for directing arch pp *)
(********************************)

type ppmode = Ascii | Latex | Dot | DotFig


let verbose = ref 0
and just_parse = ref false
and switch = ref false
and switchelse = ref false
and topos_ext = ref true
and modelcondition = ref false

(* Override condition *)
type cond = CondFile | CondTrue | CondSC

let cond = ref CondFile

let parse_cond = function
  | "file" -> Some CondFile
  | "true" -> Some CondTrue
  | "sc" -> Some CondSC
  | _    -> None

let pp_cond = function
  | CondFile -> "file"
  | CondTrue -> "true"
  | CondSC -> "sc"

(*******************)
(* Axiomatic model *)
(*******************)

(* kind of *)
type axiom_kind = Global | Performed | Dmb | GlobalPerformed
type axiomatic = AxiomNot | Axiom of axiom_kind
let axiomatic = ref (Axiom Global)

let parse_axiomatic = function
  | "no" ->  Some AxiomNot
  | "global" ->  Some (Axiom Global)
  | "performed"|"perf" -> Some (Axiom Performed)
  | "dmb" -> Some (Axiom Dmb)
  | "gp" -> Some (Axiom GlobalPerformed)
  | _ -> None

let pp_axiomatic = function
  |AxiomNot -> "no"
  |Axiom Global -> "global"
  |Axiom Performed -> "performed"
  |Axiom Dmb -> "dmb"
  |Axiom GlobalPerformed -> "gp"

(* How we reject out of thin air & friends *)

type thinair = Tnone | Tppoext | Trwext | Tcausal

let pp_thinair m = match m with
| Tnone -> "none"
| Tppoext -> "ppoext"
| Trwext -> "rwext"
| Tcausal -> "causal"

let parse_thinair m = match m with
| "none" -> Some Tnone
| "ppoext"|"ppo" -> Some Tppoext
| "rwext"|"rw" -> Some Trwext
| "causal" -> Some Tcausal
| _ -> None

(* PPO variation after CAV *)
type ppo = Pcav | Pdd | Ppso


let pp_ppo m = match m with
| Pcav -> "cav"
| Pdd -> "dd"
| Ppso -> "pso"

let parse_ppo m = match m with
| "cav" -> Some Pcav
| "dd" -> Some Pdd
| "pso" -> Some Ppso
| _ -> None


(* Rf handling by axiomatic model *)
type rfkind =
  | RFAll  (* Rf are global *)
  | RFExt  (* External rf are global (a la TSO) *)
  | RFNone (* No rf is global (a la PPC) *)

let pp_rfkind k = match k with
  | RFAll -> "all"
  | RFExt -> "ext"
  | RFNone -> "none"

let parse_rfkind s = match s with
  | "all" -> Some RFAll
  | "ext" -> Some RFExt
  | "none" -> Some RFNone
  | _ -> None


(* Uniproc selection *)
type uniproc = UniAll | LoadLoadHasard

let pp_uniproc k = match k with
| UniAll -> "all"
| LoadLoadHasard -> "llh"

let parse_uniproc s = match s with
| "all" -> Some UniAll
| "llh" -> Some LoadLoadHasard
| _ -> None

(* B-extension *)
type bextkind =
  | BExtPPO (* Extend ppo *)
  | BExtRW  (* Extend any po RW pair *)
  | BExtNone

let pp_bextkind k = match k with
| BExtPPO -> "ppo"
| BExtRW -> "rw"
| BExtNone -> "none"

let parse_bextkind s = match s with
| "ppo" -> Some BExtPPO
| "rw"  -> Some BExtRW
| " none" -> Some BExtNone
| _     -> None

(* lwsync cumulativity *)
type lwsync = AB | B

let pp_lwsync = function
  | AB -> "ab"
  | B  -> "b"

let parse_lwsync = function
  | "ab"|"AB" -> Some AB
  | "b" | "B" -> Some B
  | _ -> None

(* Reject non-sc silently *)
type sc_check =
  | NoCheck
  | VoCheck
  | RfmapCheck

let sc_check = ref NoCheck

let parse_sc_check = function
  | "no" -> Some NoCheck
  | "vo" -> Some VoCheck
  | "rfm" -> Some RfmapCheck
  | _ -> None

let pp_sc_check = function
  | NoCheck -> "no"
  | VoCheck -> "vo"
  | RfmapCheck -> "rfm"

(* Filter out "precisely" preserved coherence order violations *)
let precise_pco = ref true


(**************)
(* File names *)
(**************)

let vos_name name_dot =
  let base = Filename.chop_extension name_dot in
  base ^ ".vos"

let dot_name name_xxx =
  let base = Filename.chop_extension name_xxx in
  base ^ ".dot"
  

(****************)
(* basic utils  *)
(****************)
external int_compare : int -> int -> int = "caml_int_compare"

let int_eq (x:int) (y:int) = x == y
let string_eq (s1:string) (s2:string) = Pervasives.(=) s1 s2

let is_none = function
  | None -> true
  | Some _ -> false

let proj_opt default = function
  | None -> default
  | Some x -> x

let app_opt f = function
  | None -> None
  | Some x -> Some (f x)

let cons x xs = x::xs

let rec last = function
  | [] -> assert false
  | [x] -> x
  | _::xs -> last xs

let rec option_map f xs = match xs with
| [] -> []
| x::xs ->
   match f x with
   | None -> option_map f xs
   | Some y -> y :: option_map f xs

let map_string f s =
  let b = Buffer.create (String.length s) in
  for k=0 to String.length s-1 do
    Buffer.add_string b (f s.[k])
  done ;
  Buffer.contents b

(***************)
(* int parsing *)
(***************)

let string_of_intkm s =
  let len = String.length s in
  try
    let x =
      match s.[len-1] with
      | 'k'|'K' -> 1000 * int_of_string (String.sub s 0 (len-1))
      | 'm'|'M' -> 1000000 * int_of_string (String.sub s 0 (len-1))
      | _ -> int_of_string s in
    Some x
  with
  | Failure "int_of_string" -> None

(******************)
(* List utilities *)
(******************)

let cons x xs = x::xs

let pp_list chan sep pp_x =
  let rec do_rec = function
    | [] -> ()
    | [x] -> pp_x chan x
    | x::xs -> pp_x chan x ; output_string chan sep ; do_rec xs in
  do_rec

let iteri f =
  let rec iter_rec i xs = match xs with
  | [] -> ()
  | x::xs -> f i x ; iter_rec (i+1) xs in
  iter_rec 0

let mapi f =
  let rec map_rec i xs = match xs with
  | [] -> []
  | x::xs -> f i x :: map_rec (i+1) xs in
  map_rec 0

let rec rev_filter p xs =
  let rec do_rec ys = function
    | [] -> ys
    | x::xs ->
	do_rec (if p x then x::ys else ys) xs in
  do_rec [] xs

let rec map3 f xs ys zs = match xs,ys,zs with
| [],[],[] -> []
| x::xs,y::ys,z::zs ->
    f x y z::map3 f xs ys zs
| _,_,_ -> assert false

let rem_dups is_same =
  let rec rem_rec prev = function
    | [] -> []
    | atom::rem ->
        if is_same prev atom then rem_rec prev rem
        else atom::rem_rec atom rem in
  fun atoms -> match atoms with
  | [] -> []
  | atom::atoms ->
      atom::rem_rec atom atoms

(* Connectors for predicates *)

let (|||) p1 p2 = fun e -> p1 e || p2 e

let (&&&) p1 p2 = fun e -> p1 e && p2 e

(************)
(* Matrices *)
(************)

(* Transposition *)
exception TransposeFailure

let transpose rows = match rows with
| [] -> raise TransposeFailure
| xs::_ ->
    let cols = List.map (fun _ -> []) xs in
    let cols =
      try
	List.fold_right (List.map2 cons) rows cols
     with Invalid_argument "List.map2" -> raise TransposeFailure in
    cols

(* Code pretty print *)

let rec iter_by_line f prog =
  let heads,prog =
    List.split
      (List.map
	 (fun xs -> match xs with
	 | x::xs -> Some x,xs
	     | [] ->None,[])
	 prog) in
  if not (List.for_all is_none heads) then begin
    f heads ;
    iter_by_line f prog
  end

let fmt_cell sz s = Printf.sprintf " %-*s " sz s

let fmt_line szs line =
  String.concat "|"
    (List.map2
       (fun sz io -> match io with
       | Some i -> fmt_cell sz i
       | None -> fmt_cell sz "")
       szs line)

let compute_sizes m =
  List.map
    (fun cs ->
      List.fold_right
        (fun i k -> max (String.length i) k)
        cs 0)
    m
  
let pp_prog chan m =
  let szs = compute_sizes m in
  iter_by_line
    (fun line -> Printf.fprintf chan "%s;\n" (fmt_line szs line))
    m

(* Code fmt *)

let string_of_prog m =
  let buff = Buffer.create 128 in
  let szs = compute_sizes m in
  iter_by_line
    (fun line -> Printf.bprintf buff "%s;\n" (fmt_line szs line))
    m ;
  Buffer.contents buff

(******)
(* IO *)
(******)

let output_protect_gen o f name =
  let chan =
    try o name
    with Sys_error msg ->
      Printf.eprintf "open_out failed: %s\n" msg ; flush stderr ;
      try open_out "/dev/null" with Sys_error _ -> assert false in
  let y =
    try f chan with e -> close_out chan ; raise e in
  close_out chan ;
  y

let input_protect_gen o f name =
  let chan =
    try o name
    with Sys_error msg ->
      raise
        (Fatal
           (Printf.sprintf "open_in failed: %s" msg)) in
  let y =
    try f chan with e -> close_in chan ; raise e in
  close_in chan ;
  y

let output_protect f name = output_protect_gen open_out f name
let input_protect f name = input_protect_gen open_in f name

(**************************)
(* Reading list of inputs *)
(**************************)

let input_line chan =
  try Some (Pervasives.input_line chan)
  with End_of_file -> None

let is_list name =
  let base = Filename.basename name in
  String.length base > 0 && base.[0] = '@'

let fconcat dir name = match dir with
| "." -> name
| _ -> Filename.concat dir name

let ignore_line line =
  String.length line = 0 ||
  (String.length line > 0 && line.[0] = '#')

let rec input_lines dir k chan =
  let rec do_rec () =
    match input_line chan with
    | Some base ->
        if ignore_line base then
          do_rec ()
        else
          let name = fconcat dir base in
          read_filenames name (do_rec ())
    | None -> k in
  do_rec ()

and read_filenames name k =
  if is_list name then
    let dir = Filename.dirname name in
    input_protect (input_lines dir k) name
  else name::k



let expand_argv names =
  List.fold_left
    (fun k name -> read_filenames name k)
    [] names

(***************************)
(* cross product iteration *)
(***************************)

let fold_cross_gen add start xss kont r =
 let rec fold_rec r ys xss = match xss with
  | [] -> kont ys r
  | xs::xss ->
      List.fold_left
	(fun r x -> fold_rec r (add x ys) xss)
	r xs in
 fold_rec r start (List.rev xss)


let fold_cross xss = fold_cross_gen cons [] xss

