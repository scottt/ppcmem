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

open Printf

let map = ref None

let check_map t =
  let back = Hashtbl.create 17 in
  let warn = ref false in
  Hashtbl.iter
    (fun k v ->
      let ks =
        try
          let ks = Hashtbl.find back v in
          warn := true ;
          ks
        with Not_found -> [] in
      Hashtbl.replace back v (k::ks))
    t ;
  if !warn then begin
    let ambiguous =
      Hashtbl.fold
        (fun v ks r -> match ks with
        | []|[_] -> r
        | _ -> (v,sprintf "{%s}" (String.concat "," ks))::r)
        back [] in
    Warn.warn_always
      "Ambiguous rename map:\n %s"
      (String.concat "\n"
         (List.map
            (fun (v,ks) -> sprintf "%s->%s" ks v)
            ambiguous))
  end
    
      
let set_map fname =
  let t =
    Misc.input_protect
      (fun chan -> LexRename.read fname chan TblRename.empty (fun s -> Some s))
      fname in
  map := Some t

let is_active () = match !map with
| None -> false
| Some _ -> true

let rename key = match !map with
| None -> key
| Some t ->
    try TblRename.find_value t key
    with Not_found -> key

      
