(**********************************************************
 * :: Representation of parsing tables
 **********************************************************)

(* The C++ implementation is designed so the sizes of the various
 * table entries can be adjusted.  I'm reflecting that design here,
 * but I am just using 'int' as the size choice everywhere, since
 * (I think) OCaml arrays don't get smaller if you use (e.g.) char.
 *
 * The way to make array of char efficiently is using strings, but
 * that's a TODO at best, for now.
 *)


(* for action entries; some places may still be called int *)
type action_entry = int

(* identifier for a state in the finite automaton *)
type state_id = int

(* entry in the goto table *)
type goto_entry = int

(* index for a terminal *)
type term_index = int

(* index for a nonterminal *)
type nt_index = int

(* index for a production *)
type prod_index = int

(* ErrorBitsEntry goes here *)


(* encode a symbol in the 'stateSymbol' map *)
(*   N+1:  terminal N *)
(*   0:    no symbol *)
(*   -N-1: nonterminal N *)
type symbol_id = int


(* collection of data needed for the online parsing algorithm *)
type t = {
  (* grammar counts *)
  numTerms : int;
  numNonterms : int;
  numProds : int;

  (* # of states in LR automaton *)
  numStates : int;

  (* action table, indexed by (state*actionCols + lookahead) *)
  actionCols : int;
  actionTable : action_entry array;

  (* goto table, indexed by (state*gotoCols + nonterm_id) *)
  gotoCols : int;
  gotoTable : goto_entry array;

  (* production info, indexed by production id *)
  prodInfo_rhsLen : int array;         (* this is 'unsigned char' array in C++ *)
  prodInfo_lhsIndex : nt_index array;

  (* map state to symbol shifted to arrive at that state *)
  stateSymbol : symbol_id array;

  (* ambiguous actions: one big list, for allocation purposes; then
   * the actions encode indices into this table; the first indexed
   * entry gives the # of actions, and is followed by that many
   * actions, each interpreted the same way ordinary 'actionTable'
   * entries are *)
  ambigTable : action_entry array;

  (* total order on nonterminals; see elkhound/parsetables.h *)
  nontermOrder : nt_index array;

  (* TODO: implement some of the table compression options? *)

  (* start state id (always 0) *)
  startState : state_id;
  
  (* index of last production to reduce *)
  finalProductionIndex : int;
}


(* signature for generated parse tables *)
module type S = sig
  val parseTables : t
end
