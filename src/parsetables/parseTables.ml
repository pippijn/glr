open ParseTablesType

type action_entry = ParseTablesType.action_entry
type state_id	= ParseTablesType.state_id
type goto_entry	= ParseTablesType.goto_entry
type term_index	= ParseTablesType.term_index
type nt_index	= ParseTablesType.nt_index
type prod_index	= ParseTablesType.prod_index
type symbol_id	= ParseTablesType.symbol_id

type t = ParseTablesType.t

let invalid_state : state_id = -1

let invalid_term : term_index = -1
let eof_term : term_index = 0

let invalid_symbol : symbol_id = 0

(**********************************************************
 * :: Operations
 **********************************************************)

let symIsTerm    (id : symbol_id) : bool       =  id > 0
let symAsTerm    (id : symbol_id) : term_index =  id - 1
let symIsNonterm (id : symbol_id) : bool       =  id < 0
let symAsNonterm (id : symbol_id) : nt_index   = -id - 1


(* -------------- ParseTables client access interface -------------- *)
let getActionEntry tables (state : state_id) (tok : term_index) =
  tables.actionTable.(state * tables.actionCols + tok)

let getActionEntry_noError tables (state : state_id) (tok : term_index) =
  getActionEntry tables state tok

let getStateSymbol tables (state : state_id) =
  tables.stateSymbol.(state)

let getAmbigEntry tables (entry : int) =
  tables.ambigTable.(entry)

let getNumProds tables =
  tables.numProds

let getNumTerms tables =
  tables.numTerms

let getFinalProductionIndex tables =
  tables.finalProductionIndex

let getStartState tables =
  tables.startState


let isShiftAction tables (code : action_entry) =
  let code = (code :> int) in
  code > 0 && code <= tables.numStates

(* needs tables for compression *)
let decodeShift tables (code : action_entry) shiftedTerminal : state_id =
  let code = (code :> int) in
  code - 1

let isReduceAction tables (code : action_entry) =
  let code = (code :> int) in
  code < 0

(* needs tables for compression *)
let decodeReduce tables (code : action_entry) (in_state : state_id) =
  let code = (code :> int) in
  -(code + 1)

let isErrorAction tables (code : action_entry) =
  let code = (code :> int) in
  code = 0

                       
(* this returns an index into the ambigTable *)
(* needs tables for compression *)
let decodeAmbigAction tables (code : action_entry) (in_state : state_id) =
  let code = (code :> int) in
  code - 1 - tables.numStates


let getGotoEntry tables (state_id : state_id) (nonterm_id : nt_index) =
  tables.gotoTable.(state_id * tables.gotoCols + nonterm_id)

(* needs tables for compression *)
let decodeGoto tables (code : goto_entry) shiftNonterminal : state_id =
  let code = (code :> int) in
  code

let getGoto tables (state_id : state_id) (nonterm_id : nt_index) : state_id =
  decodeGoto tables (getGotoEntry tables state_id nonterm_id) nonterm_id


let getProdInfo_rhsLen tables rule =
  tables.prodInfo_rhsLen.(rule)

let getProdInfo_lhsIndex tables rule =
  tables.prodInfo_lhsIndex.(rule)


let getNontermOrdinal tables idx =
  tables.nontermOrder.(idx)


(* OCaml'ish, but slower interface *)

type action_kind =
  | Shift
  | Reduce
  | Error
  | Ambiguous

type action =
  | ShiftAction of (*new_state:*)state_id
  | ReduceAction of (*production:*)prod_index
  | AmbiguousAction of (*start:*)int
  | ErrorAction


let kind_of_action tables (code : action_entry) =
  if isReduceAction tables code then
    Reduce
  else if isErrorAction tables code then
    Error
  else if isShiftAction tables code then
    Shift
  else
    Ambiguous



let getAction tables (state : state_id) (tok : term_index) =
  let code = getActionEntry tables state tok in
  if isReduceAction tables code then
    ReduceAction (decodeReduce tables code state)
  else if isErrorAction tables code then
    ErrorAction
  else if isShiftAction tables code then
    ShiftAction (decodeShift tables code tok)
  else
    AmbiguousAction (decodeAmbigAction tables code state)


let iter_terms tables f =
  for i = 0 to getNumTerms tables - 1 do
    f i
  done
