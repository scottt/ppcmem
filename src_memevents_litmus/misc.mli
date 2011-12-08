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

(***********************************************************)
(* Raised to finish the current test in various situations *)
(***********************************************************)

exception Exit of string
exception UserError of string
exception Fatal of string

(* Current architecture *)
type arch =
  | X86
  | PPC
  | ARM

val parse_arch : string -> arch option
val lex_arch : string -> arch
val pp_arch : arch -> string

(**********)
(* ppmode *)
(**********)

type ppmode = Ascii | Latex | Dot | DotFig

(***********************)
(* Non-managed options *)
(***********************)

val verbose : int ref
val just_parse : bool ref

(* For switching something *)
val switch : bool ref
(* and something else *)
val switchelse : bool ref

(***********************)
(* Definitive switches *)
(***********************)

val modelcondition : bool ref

(* Overide condition *)
type cond = CondFile | CondTrue | CondSC
val cond : cond ref
val parse_cond : string -> cond option
val pp_cond : cond -> string

(*******************************)
(* Axiomatic model refinements *)
(*******************************)

(* kind of *)
type axiom_kind =
  | Global (* performed4 model *)
  | Performed (* Equivalent? forumulation with performed w.r.t. *)
  | Dmb       (* ARM formulation *)
  | GlobalPerformed (* performed4 + per proc check *)

type axiomatic = AxiomNot | Axiom of axiom_kind
val axiomatic : axiomatic ref
val parse_axiomatic : string -> axiomatic option
val pp_axiomatic : axiomatic -> string

(* how to reject values out of thin air *)
type thinair =
  | Tnone   (* do nothing *)
  | Tppoext (* use ppo extension (ppo ; rf -> ppo) *)
  | Trwext  (* use generalized extension (RW \cap po \cap ghb ; rf -> ghb) *)
  | Tcausal (* reject (ppo ; rf)+ loops *)

val pp_thinair : thinair -> string
val parse_thinair : string -> thinair option

(* PPO variation after CAV *)
type ppo =
  | Pcav (* As in Cav submission, ie, Ctr/Dd identical *)
  | Pdd  (* Improved, perform PosWR union + transitive closure on dd only *)
  | Ppso (* A la pso, ie R* is in ppo *)

val pp_ppo : ppo -> string
val parse_ppo : string -> ppo option



(* Rf handling by axiomatic model *)
type rfkind =
  | RFAll  (* Rf are global *)
  | RFExt  (* External rf are global (a la TSO) *)
  | RFNone (* No rf is global (a la PPC) *)

val pp_rfkind : rfkind -> string
val parse_rfkind : string -> rfkind option

(* Uniproc selection *)
type uniproc = UniAll | LoadLoadHasard

val pp_uniproc : uniproc -> string
val parse_uniproc : string -> uniproc option

(* lwsync cumulativity *)
type lwsync = AB | B

val pp_lwsync : lwsync -> string

val parse_lwsync : string -> lwsync option


(* Use extended topological orders generator *)
val topos_ext : bool ref

(* Filter out "precisely" preserved coherence order violations *)
val precise_pco : bool ref

(* Reject non-sc silently *)
type sc_check =
  | NoCheck (* Don't *)
  | VoCheck (* By analyzing view orders *)
  | RfmapCheck (* By analyzing rfmap + write serialization *)

val sc_check : sc_check ref
val parse_sc_check : string -> sc_check option
val pp_sc_check : sc_check -> string

(**************)
(* File names *)
(**************)
val vos_name : string -> string
val dot_name : string -> string

(****************)
(* basic misc   *)
(****************)
external int_compare : int -> int -> int = "caml_int_compare"
val int_eq : int -> int -> bool
val string_eq : string -> string -> bool


val is_none : 'a option -> bool
val proj_opt : 'a -> 'a option -> 'a
val app_opt : ('a -> 'b) -> 'a option -> 'b option
val option_map : ('a -> 'b option) -> 'a list -> 'b list
val map_string : (char -> string) -> string -> string


(* Generalize int parsing *)
val string_of_intkm : string -> int option

(* Some useful function on lists *)
val cons : 'a -> 'a list -> 'a list
val last : 'a list -> 'a
val pp_list :
  out_channel -> string -> (out_channel -> 'a -> unit) -> 'a list -> unit
val iteri : (int -> 'a -> unit) -> 'a list -> unit
val mapi : (int -> 'a -> 'b) -> 'a list -> 'b list
val rev_filter : ('a -> bool) -> 'a list -> 'a list
val map3 :
    ('a -> 'b -> 'c -> 'd) -> 
      'a list -> 'b list -> 'c list -> 'd list

(* Remove duplicates, according to equality function,
   WARNING, correct only when duplicates are in sequence *)
val rem_dups : ('a -> 'a -> bool) -> 'a list -> 'a list

(* Lift boolean connectors to predicates *)
val (|||) : ('a -> bool) -> ('a -> bool) -> 'a -> bool
val (&&&) : ('a -> bool) -> ('a -> bool) -> 'a -> bool

(******************)
(* Matrix helpers *)
(******************)

(* Fails on empty or non regular matrices *)
exception TransposeFailure
val transpose : 'a list list -> 'a list list

(* Pretty print (code) matrices *)
val pp_prog : out_channel -> string list list -> unit
val string_of_prog : string list list -> string

(***************)
(* I/O helpers *)
(***************)

(* Call: 'ouput_protect f name'
   Does apply f to a channel chan resulting
   from opening file name.
   Channel chan is closed, even if f raises an exception.
   The exception is re-raised *)
val output_protect : (out_channel -> 'a) -> string -> 'a

(* Idem for input, raises Fatal msg if file cannot be opened *)
val input_protect : (in_channel -> 'a) -> string -> 'a

(* Generic versions, with opening functions as argumentrs *)
val output_protect_gen :
    (string -> out_channel) -> (out_channel -> 'a) -> string -> 'a
val input_protect_gen :
    (string -> in_channel) -> (in_channel -> 'a) -> string -> 'a


(****************************)
(* Expand command line args *)
(****************************)

(* File names in style '@name' are replaced by their contents *)

val expand_argv : string list -> string list

(****************************)
(* Cross product generators *)
(****************************)

(* Generate all elts in size n cross product and
   apply a function to them.

   'fold_cross  [xs1 ; ... xsN] f v0'
     computes f p1 (f p2 ... (f pM v0)), where the pi's are
     all lists [y1 ; ... yN] with
       y1,..., yN in (xs1 X ... X xsN)
*)
val fold_cross :  'a list list ->  ('a list -> 'b -> 'b) -> 'b -> 'b

(* Generalized cross product fold
   'fold_cross  add y0 [xs1 ; ... xsN] f v0
    computes  f p1 (f p2 ... (f pM v0)), as aboves where the pi's
    are 'add y1 (.. (add yN y0))'.

   Notice that fold_cross is
   fold_cross_gen (fun y ys -> y::ys) [] *)
val fold_cross_gen :
    ('a -> 'b -> 'b) -> 'b -> 'a list list -> ('b -> 'c -> 'c) -> 'c -> 'c

