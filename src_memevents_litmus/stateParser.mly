/*********************************************************************/
/*                        Memevents                                  */
/*                                                                   */
/* Jade Alglave, Luc Maranget, INRIA Paris-Rocquencourt, France.     */
/* Susmit Sarkar, Peter Sewell, University of Cambridge, UK.         */
/*                                                                   */
/*  Copyright 2010 Institut National de Recherche en Informatique et */
/*  en Automatique and the authors. All rights reserved.             */
/*  This file is distributed  under the terms of the Lesser GNU      */
/*  General Public License.                                          */
/*********************************************************************/

%{
open SymbConstant
open MiscParser
open ConstrGen
%}

%token EOF
%token <int> PROC
%token <string> SYMB_REG
%token <string> NAME
%token <int> NUM

%token TRUE
%token EQUAL PLUS_DISJ
%token FINAL FORALL EXISTS NOT AND OR IMPLIES CASES WITH
%token LOCATIONS STAR
%token LBRK RBRK LPAR RPAR SEMI COLON 

%left PLUS_DISJ OR 
%left AND
%right IMPLIES
%nonassoc NOT

%type <MiscParser.state> init
%start init
%type <MiscParser.location> location
%start location
%type <(MiscParser.location * MiscParser.run_type) list * MiscParser.constr * (string * MiscParser.quantifier) list> constraints
%start constraints
%type  <MiscParser.constr> constr
%start constr
%%

/* For initial state */
init:
| atom_semi_list EOF { $1 }


reg:
| NAME     {  $1 }

maybev:
| NUM  { Concrete $1 }
| NAME { Symbolic $1 }

location:
| PROC COLON reg  {Location_reg ($1,$3)}
| NUM COLON reg   {Location_reg ($1,$3)}
| SYMB_REG        {Location_sreg $1 }
| LBRK maybev RBRK {Location_global $2}
/* Hum, for backward compatibility, and compatiility with printer */
| maybev { Location_global $1 } 

atom:
| location EQUAL maybev {($1,$3)}
    
atom_semi_list: 
| {[]}
| SEMI {[]}
| atom {$1::[]}
| atom SEMI atom_semi_list  {$1::$3}

	
/* For final state constraints */

loc_typ:
| location { ($1,I) }
| location STAR { ($1,P) }

loc_semi_list: 
| {[]}
| SEMI {[]}
| loc_typ {$1::[]}
| loc_typ SEMI loc_semi_list  {$1::$3}

locations:
|  LOCATIONS LBRK loc_semi_list RBRK { $3 }
| { [] }

constraints : 
| locations old_constraints
  { let x = $1 in
    let y,z = $2 in
    x,y,z }

old_constraints : 
| final EOF { $1,[] }
| final WITH kinds EOF { $1,$3 }


kinds : 
| kind         { [$1] }
| kind SEMI    { [$1] }
| kind SEMI kinds   { $1 :: $3 }

kind:
| NAME COLON FORALL { ($1,Require) }
| NAME COLON EXISTS { ($1,Allow) }
| NAME COLON NOT EXISTS { ($1,Forbid) }


final:
| constr { $1 }
| constr SEMI { $1 }

constr:
|  { ForallStates (And []) }
| FORALL prop
    {ForallStates $2}
| EXISTS prop
    {ExistsState $2}
| NOT EXISTS prop
	{ NotExistsState $3 }
| FINAL prop 
        { QueryState $2 }
| LPAR prop RPAR
    {QueryState $2}

prop: 
| 
    {And []}
| TRUE
    {And []}
| atom
    {let loc,v = $1 in Atom (loc,v) }
| NOT prop 
    {Not $2}
| prop AND prop 
    {And [$1;$3]}
| prop OR  prop 
    {Or [$1;$3]}
| prop IMPLIES prop
    { Implies ($1,$3) }
| LPAR prop RPAR 
    { $2 }

