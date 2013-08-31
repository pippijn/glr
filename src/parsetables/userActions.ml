(* see license.txt for copyright and terms of use *)
(* interface for user-defined reduction (etc.) actions *)
(* based on elkhound/useract.h *)

(* the comments below are guidelines on writing grammar actions, since
 * those grammar actions are composed to form the single-entry
 * functions documented below *)


type functions = {
  reductionActionArray		: (SemanticValue.t array -> Lexing.position -> Lexing.position -> SemanticValue.t) array;
  duplicateTerminalValueArray	: (SemanticValue.t -> SemanticValue.t) array;
  duplicateNontermValueArray	: (SemanticValue.t -> SemanticValue.t) array;
  deallocateTerminalValueArray	: (SemanticValue.t -> unit) array;
  deallocateNontermValueArray	: (SemanticValue.t -> unit) array;
  showTerminalValueArray	: (SemanticValue.t -> string) array;
  showNontermValueArray		: (SemanticValue.t -> string) array;
  mergeAlternativeParsesArray	: (SemanticValue.t -> SemanticValue.t -> SemanticValue.t) array;
  keepNontermValueArray		: (SemanticValue.t -> bool) array;
  reclassifyTokenArray		: (SemanticValue.t -> int) array;
}

let default_dup      (names : string array) (sym : int) (sval : SemanticValue.t) : SemanticValue.t =
  if PtreeOptions._dup_clone () then
    let serialised = Marshal.(to_string sval [No_sharing]) in
    Marshal.(from_string serialised 0)
  else
    sval

let default_del      (names : string array) (sym : int) (sval : SemanticValue.t) : unit = ()
let default_show     (names : string array) (sym : int) (sval : SemanticValue.t) : string = ""
let default_merge    (names : string array) (sym : int) (left : SemanticValue.t) (right : SemanticValue.t) : SemanticValue.t =
  Printf.printf "warning: no function to merge nonterminal %s\n" names.(sym); left
let default_keep     (names : string array) (sym : int) (sval : SemanticValue.t) : bool = true
let default_classify (names : string array) (oldTokenType : int) (sval : SemanticValue.t) : int = oldTokenType


(* package of functions; the user will create an instance of this
 * record, and the parser will carry it along to invoke the various
 * action functions *)
type 'result t = {
  (* user-supplied reduction actions
   *  - production 'id' is being used to reduce
   *  - 'svals' contains an array of semantic values yielded by the RHS
   *    symbols, such that the 0th element is the leftmost RHS element;
   *    the pointers in the array are owner pointers (the array ptr itself
   *    is a serf)
   *  - 'loc' is the location of the left edge of the parse subtree
   *  - this fn returns the semantic value for the reduction; this return
   *    value is an owner pointer *)
  reductionAction :
    (*context?*)
    int ->			(* production being used to reduce *)
    SemanticValue.t array ->	(* array of svals for RHS symbols *)
    Lexing.position ->		(* start position *)
    Lexing.position ->		(* end position *)
    SemanticValue.t;		(* sval for the reduction *)

  (* duplication of semantic values:
   *  - the given 'sval' is about to be passed to a reduction action
   *    function.  the user must return a value to be stored in place
   *    of the old one, in case it is needed to pass to another action
   *    function in case of local ambiguity; 'sval' is a serf
   *  - the return value will be yielded (if necessary) to the next
   *    consumer action function, and is an owner ptr
   *  - some possible strategies:
   *    - return NULL, in which case it is probably an error for the
   *      value to be passed to another action (i.e. the grammar needs
   *      to be LALR(1) near this semantic value); in this case, 'del'
   *      will not be called on the NULL value
   *    - increment a reference count and return 'sval'
   *    - do nothing, and rely on some higher-level allocation scheme
   *      such as full GC, or regions *)
  duplicateTerminalValue :
    (*context?*)
    ParseTables.term_index ->	(* terminal id *)
    SemanticValue.t ->		(* sval being yielded *)
    SemanticValue.t;		(* sval to yield next time *)
  duplicateNontermValue :
    (*context?*)
    ParseTables.nt_index ->	(* nonterminal id *)
    SemanticValue.t ->		(* sval being yielded *)
    SemanticValue.t;		(* sval to yield next time *)

  (* a semantic value didn't get passed to an action function, either
   * because it was never used at all (e.g. a semantic value for a
   * punctuator token, which the user can simply ignore), or because we
   * duplicated it in anticipation of a possible local ambiguity, but
   * then that parse turned out not to happen, so we're cancelling
   * the dup now; 'sval' is an owner pointer *)
  deallocateTerminalValue :
    (*context?*)
    ParseTables.term_index ->	(* terminal id *)
    SemanticValue.t ->		(* sval being dropped *)
    unit;
  deallocateNontermValue :
    (*context?*)
    ParseTables.nt_index ->	(* nonterminal id *)
    SemanticValue.t ->		(* sval being dropped *)
    unit;

  (* function to create a string representation of the semantic value,
   * used in debugging and traces *)
  showTerminalValue :
    (*context?*)
    ParseTables.term_index ->	(* terminal id *)
    SemanticValue.t ->		(* sval to print *)
    string;
  showNontermValue :
    (*context?*)
    ParseTables.nt_index ->	(* nonterminal id *)
    SemanticValue.t ->		(* sval to print *)
    string;

  (* this is called when there are two interpretations for the same
   * sequence of ground terminals, culminating in two different reductions
   * deriving the same left-hand-side nonterminal (identified by 'ntIndex');
   * it should return a value to be used in the place where they conflict'
   * both 'left' and 'right' are owner pointers, and the return value
   * is also an owner pointer
   *
   * NOTE: the 'left' value is always the node which came first, and
   * might even have been yielded to another reduction already
   * (depending on the grammar), whereas the 'right' value is always a
   * node which was just created, and has definitely *not* been
   * yielded to anything (this fact is critical to solving the general
   * yield-then-merge problem) *)
  mergeAlternativeParses :
    ParseTables.nt_index ->	(* nonterminal with two derivations *)
    SemanticValue.t ->		(* sval from derivation 1 *)
    SemanticValue.t ->		(* sval from derivation 2 *)
    SemanticValue.t;		(* merged sval *)

  (* after every reduction, the semantic value is passed to this function,
   * which returns 'false' if the reduction should be cancelled; if it
   * does return false, then 'sval' is an owner pointer (the parser engine
   * will drop the value on the floor) *)
  keepNontermValue :
    ParseTables.nt_index ->	(* reduced nonterm id *)
    SemanticValue.t ->		(* sval that 'reductionAction' yielded *)
    bool;			(* if false, drop the sval on the floor *)

  (* every time a token is pulled from the lexer, this reclassifier is
   * used to give the user a chance to reinterpret the token, before it
   * is used for reduction lookahead comparisons; it returns the
   * reclassified token type, or 'oldTokenType' to leave it unchanged *)
  reclassifyToken :
    ParseTables.term_index ->
    SemanticValue.t ->
    ParseTables.term_index;

  (* descriptions of symbols with their semantic values; this is useful
   * for the ACTION_TRACE function of the parser *)
  terminalDescription : ParseTables.term_index -> SemanticValue.t -> string;
  nonterminalDescription : ParseTables.nt_index -> SemanticValue.t -> string;

  (* get static names for all of the symbols *)
  terminalName : ParseTables.term_index -> string;
  terminalAlias : ParseTables.term_index -> string;
  nonterminalName : ParseTables.nt_index -> string;
}


let make_trivial (underlying : 'a t) : unit t = { underlying with
  reductionAction = (fun _ _ _ _ -> SemanticValue.null);
  duplicateTerminalValue = (fun _ a -> a);
  duplicateNontermValue = (fun _ a -> a);
  deallocateTerminalValue = (fun _ _ -> ());
  deallocateNontermValue = (fun _ _ -> ());
  showTerminalValue = (fun _ _ -> "");
  showNontermValue = (fun _ _ -> "");
  mergeAlternativeParses = (fun _ l r -> l);
  keepNontermValue = (fun _ _ -> true);
  reclassifyToken = (fun oldTokenType _ -> oldTokenType);
}


(* signature for generated user actions *)
module type S = sig
  type result
  val userActions : result t
end


exception Cancel of string

let cancel reason =
  print_endline ("cancel: " ^ reason);
  raise (Cancel reason)
