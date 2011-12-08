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

(* All names associated with a test *)
type t =
  {
   name : string ; (* key, whether extracted from file name or from file content *)
   humanname : string ; (* to use in articles *)
   file : string ; (* read from that file *)
   texname : string ; (* to derive latex commands *)
   doc : string ;   (* Some limited info, carried to latex *)
  }
    
