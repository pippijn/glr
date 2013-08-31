(* Implementation Notes
 *
 * A design point: [GLR] uses more 'global's than I do.  My criteria
 * here is that something should be global (stored in type glr) if
 * it has meaning between processing of tokens.  If something is only
 * used during the processing of a single token, then I make it a
 * parameter where necessary.
 *
 * Update: I've decided to make 'currentToken' and 'parserWorklist'
 * global because they are needed deep inside of 'glrShiftNonterminal',
 * though they are not needed by the intervening levels, and their
 * presence in the argument lists would therefore only clutter them.
 *
 * Description of the various lists in play here:
 *
 *   active_parsers
 *   --------------
 *   The active parsers are at the frontier of the parse tree
 *   space.  It *never* contains more than one stack node with
 *   a given parse state; I call this the unique-state property
 *   (USP).  If we're about to add a stack node with the same
 *   state as an existing node, we merge them (if it's a shift,
 *   we add another leftAdjState; if it's a reduction, we add a
 *   rule node *and* another leftAdjState).
 *
 *   Before a token is processed, active_parsers contains those
 *   parsers that successfully shifted the previous token.  This
 *   list is then walked to make the initial reduction worklist.
 *
 *   Before the shifts are processed, the active_parsers list is
 *   cleared.  As each shift is processed, the resulting parser is
 *   added to active_parsers (modulo USP).
 *
 *
 * Discussion of path re-examination, called do-limited-reductions by
 * [GLR]:
 *
 * After thinking about this for some time, I have reached the conclusion
 * that the only way to handle the problem is to separate the collection
 * of paths from the iteration over them.
 *
 * Here are several alternative schemes, and the reasons they don't
 * work:
 *
 *   1. [GLR]'s approach of limiting re-examination to those involving
 *      the new link
 *
 *      This fails because it does not prevent re-examined paths
 *      from appearing in the normal iteration also.
 *
 *   2. Modify [GLR] so the new link can't be used after the re-examination
 *      is complete
 *
 *      Then if *another* new link is added, paths involving both new
 *      links wouldn't be processed.
 *
 *   3. Further schemes involving controlling which re-examination stage can
 *      use which links
 *
 *      Difficult to reason about, unclear a correct scheme exists, short
 *      of the full-blown path-listing approach I'm going to take.
 *
 *   4. My first "fix" which assumes there is never more than one path to
 *      a given parser
 *
 *      This is WRONG.  There can be more than one path, even as all such
 *      paths are labeled the same (namely, with the RHS symbols).  Consider
 *      grammar "E -> x | E + E" parsing "x+x+x": both toplevel parses use
 *      the "E -> E + E" rule, and both arrive at the root parser
 *
 * So, the solution I will implement is to collect all paths into a list
 * before processing any of them.  During path re-examination, I also will
 * collect paths into a list, this time only those that involve the new
 * link.
 *
 * This scheme is clearly correct, since path collection cannot be disrupted
 * by the process of adding links, and when links are added, exactly the new
 * paths are collected and processed.  It's easy to see that every path is
 * considered exactly once.
 *
 *
 * MAJOR UPDATE (12/06/02):  I've replaced the state worklist (SWL) core
 * used in all previous GLR implementations with a reduction worklist (RWL)
 * core.  This core is just as fast, but can be implemented to always
 * avoid the yield-then-merge problem for acyclic grammars.
 *
 *
 * Below, parse-tree building activity is marked "TREEBUILD".
 *)


(* Relative to C++ implementation, what is not done:
 *   - Table compression
 *   - Heavy testing of the mini-LR core
 *)


module Colour = TermColour.Make(TermColour.HTML)

(* We define our own versions of these exceptions, so that user code raising
 * the ones in Pervasives will not interfere with parser internals. *)
exception End_of_file

(* These exceptions are part of the public interface. *)
exception ParseError of ParseTables.state_id * (*token*)ParseTables.term_index
exception Located of SourceLocation.t * exn * string


let keep_cancel () =
  UserActions.cancel "keep() returned false"


(* ------------------ accounting statistics ----------------- *)
type statistics = {
  mutable numStackNodesAllocd	: int;
  mutable maxStackNodesAllocd	: int;
  mutable detShift		: int;
  mutable detReduce		: int;
  mutable nondetShift		: int;
  mutable nondetReduce		: int;
}


(* link from one stack node to another *)
type sibling_link = {
  (* stack node we're pointing at; == StackNode.cNULL if none *)
  mutable sib : stack_node;

  (* semantic value on this link *)
  mutable sval : SemanticValue.t;

  (* source locations *)
  mutable start_p : Lexing.position;
  mutable end_p : Lexing.position;

  (* possible TODO: yield count *)
}

(* node in the GLR graph-structured stack; all fields are
 * mutable because these are stored in a pool for explicit re-use *)
and stack_node = {
  (* LR parser state when this node is at the top *)
  mutable state : ParseTables.state_id;

  (* pointers to adjacent (to the left) stack nodes *)
  (* possible TODO: put links into a pool so I can deallocate them *)
  mutable leftSiblings : sibling_link list;

  (* logically first sibling in the sibling list; separated out
   * from 'leftSiblings' for performance reasons *)
  mutable firstSib : sibling_link;

  (* number of sibling links pointing at this node, plus the
   * number of worklists this node appears in *)
  mutable referenceCount : int;

  (* number of links we can follow to the left before hitting a node
   * that has more than one sibling *)
  mutable determinDepth : int;

  (* position of token that was active when this node was created
   * (or pulled from pool); used in yield-then-merge calculations *)
  mutable column : int;
}

(* this is a path that has been queued for reduction;
 * all fields mutable to support pooling *)
type path = {
  (* array of sibling links, i.e. the path; 0th element is
   * leftmost link *)
  sibLinks : sibling_link array;

  (* corresponding array of symbol ids to interpret svals *)
  symbols : ParseTables.symbol_id array;

  (* rightmost state's id *)
  mutable startStateId : ParseTables.state_id;

  (* production we're going to reduce with *)
  mutable prodIndex : int;

  (* number of right hand side symbols in this production *)
  mutable rhsLen : int;

  (* column from leftmost stack node *)
  mutable startColumn : int;

  (* the leftmost stack node itself *)
  mutable leftEdgeNode : stack_node;

  (* next path in dequeueing order *)
  mutable next : path option;
}


(* GLR parser object *)
type 'result glr = {
  (* top of priority queue of reduction paths *)
  mutable top : path option;

  (* parse tables from the grammar *)
  tables : ParseTables.t;

  (* user-specified actions *)
  userAct : 'result UserActions.t;

  (* treat this as a local variable of RWL.processWorklist, included
   * here just to avoid unnecessary repeated allocation *)
  toPass : SemanticValue.t array;

  (* pool of path objects *)
  pathPool : path Objpool.t;

  (* set of topmost parser nodes *)
  active_parsers : stack_node Arraystack.t;

  (* swapped with 'active_parsers' periodically, for performance reasons *)
  prev_active : stack_node Arraystack.t;

  (* node allocation pool; shared with GLR.parseToken *)
  stackNodePool : stack_node Objpool.t;

  (* current token number *)
  mutable globalNodeColumn : int;

  (* parser action statistics *)
  stats : statistics;
}


let stats glr =
  glr.stats


(* produce a small unique integer for each
 * physically different semantic value *)
let colour_id id = Colour.pink ("@" ^ string_of_int id)
let empty_unique_table = [(SemanticValue.null, colour_id 0)]
let unique_table = ref empty_unique_table
let unique_id : SemanticValue.t -> string =
  fun obj ->
    try
      snd (List.find (fun (addr, _) -> addr == obj) !unique_table)
    with Not_found ->
      let id = colour_id (List.length !unique_table) in
      unique_table := (obj, id) :: !unique_table;
      id


let cSTATE_INVALID = ParseTables.cSTATE_INVALID


(**********************************************************
 * :: Front ends to user code
 **********************************************************)

module User = struct

  open UserActions


  let terminalName userAct tokType =
    Colour.yellow (
      if Options._terminal_names () then
        userAct.terminalName tokType
      else
        userAct.terminalAlias tokType
    )


  let nonterminalName userAct nonterm =
    Colour.yellow (userAct.nonterminalName nonterm)


  let symbolName userAct sym =
    assert (sym <> ParseTables.cSYMBOL_INVALID);

    if ParseTables.symIsTerm sym then
      terminalName userAct (ParseTables.symAsTerm sym)
    else
      nonterminalName userAct (ParseTables.symAsNonterm sym)


  let showTerminalValue userAct term sval =
    Colour.escape (userAct.showTerminalValue term sval)

  let showNontermValue userAct nonterm sval =
    Colour.escape (userAct.showNontermValue nonterm sval)

  let showSemanticValue userAct sym sval =
    assert (sym <> ParseTables.cSYMBOL_INVALID);

    if ParseTables.symIsTerm sym then
      showTerminalValue userAct (ParseTables.symAsTerm sym) sval
    else
      showNontermValue userAct (ParseTables.symAsNonterm sym) sval


  let reductionAction userAct productionId svals start_p end_p =
    userAct.reductionAction productionId svals start_p end_p


  let mergeAlternativeParses userAct lhsIndex left right =
    if Options._trace_parse () then (
      Printf.printf "%s %s\n1: %s %s\n2: %s %s\n"
        (Colour.green "merge")
        (nonterminalName userAct lhsIndex)
        (unique_id left)
        (showNontermValue userAct lhsIndex left)
        (unique_id right)
        (showNontermValue userAct lhsIndex right);
      flush stdout;
    );

    let merged = userAct.mergeAlternativeParses lhsIndex left right in

    if Options._trace_parse () then (
      Printf.printf "=: %s %s\n"
        (unique_id merged)
        (showNontermValue userAct lhsIndex merged);
      flush stdout;
    );

    merged


  let keepNontermValue userAct lhsIndex sval =
    userAct.keepNontermValue lhsIndex sval


  let duplicateTerminalValue userAct term sval =
    userAct.duplicateTerminalValue term sval

  let duplicateNontermValue userAct nonterm sval =
    userAct.duplicateNontermValue nonterm sval

  let duplicateSemanticValue userAct sym sval =
    assert (sym <> ParseTables.cSYMBOL_INVALID);

    let copy =
      (* The C++ implementation checks for NULL sval, but that doesn't
       * make sense in the ML version, as a zero value for an sval is
       * perfectly safe and should be allowed. *)
      if ParseTables.symIsTerm sym then
        duplicateTerminalValue userAct (ParseTables.symAsTerm sym) sval
      else
        duplicateNontermValue userAct (ParseTables.symAsNonterm sym) sval
    in

    if Options._trace_parse () then (
      Printf.printf "%s %s%s = %s %s\n"
        (Colour.green "dup")
        (symbolName userAct sym)
        (unique_id sval)
        (unique_id copy)
        (showSemanticValue userAct sym sval);
      flush stdout;
    );

    copy


  let deallocateTerminalValue userAct term sval =
    userAct.deallocateTerminalValue term sval

  let deallocateNontermValue userAct nonterm sval =
    userAct.deallocateNontermValue nonterm sval

  let deallocateSemanticValue userAct sym sval =
    assert (sym <> ParseTables.cSYMBOL_INVALID);

    if Options._trace_parse () then (
      Printf.printf "%s %s%s %s\n"
        (Colour.green "del")
        (symbolName userAct sym)
        (unique_id sval)
        (showSemanticValue userAct sym sval);
      flush stdout;
    );

    if ParseTables.symIsTerm sym then
      deallocateTerminalValue userAct (ParseTables.symAsTerm sym) sval
    else
      deallocateNontermValue userAct (ParseTables.symAsNonterm sym) sval


  let reclassifyToken userAct lexer token =
    let open Lexerint in
    (* get original type/sval/sloc *)
    let tokType = lexer.index token in
    let tokSval = lexer.sval token in
    let tokSloc = lexer.sloc token in

    (* reclassify type *)
    let tokType = userAct.reclassifyToken tokType tokSval in

    (* return all token properties *)
    tokType, tokSval, tokSloc

end


(**********************************************************
 * :: Parallel parsers
 **********************************************************)

module SiblingLink = struct

  type t = sibling_link

  (* NULL sibling link *)
  let cNULL : sibling_link = Obj.magic ()

  let create sib sval start_p end_p = { sib; sval; start_p; end_p; }

end


(**********************************************************
 * :: Single parser node
 **********************************************************)

module StackNode : sig

  type t = stack_node

  val cNULL : t

  val create : unit -> t

  val init : t -> ParseTables.state_id -> unit

  val make
    : 'result glr
    -> ParseTables.state_id
    -> stack_node

  val incRefCt : t -> unit
  val decRefCt : 'result glr -> t -> unit

  val getNodeSymbol
    : 'result glr
    -> t
    -> ParseTablesType.symbol_id
  val getLinkTo
    : t
    -> t
    -> SiblingLink.t option
  val getUniqueLink
    : t
    -> SiblingLink.t

  val addFirstSiblingLink_noRefCt
    : t
    -> t
    -> SemanticValue.t
    -> Lexing.position
    -> Lexing.position
    -> unit

  val addSiblingLink
    : t
    -> t
    -> SemanticValue.t
    -> Lexing.position
    -> Lexing.position
    -> SiblingLink.t

  val computeDeterminDepth : t -> int
  val checkLocalInvariants : t -> bool

end = struct

  type t = stack_node

  (* NULL stack node *)
  let cNULL : stack_node = Obj.magic ()


  let create () = {
    state          = cSTATE_INVALID;
    leftSiblings   = [];
    firstSib       = SiblingLink.create cNULL SemanticValue.null Lexing.dummy_pos Lexing.dummy_pos;
    referenceCount = 0;
    determinDepth  = 0;
    column         = 0;
  }


  let getNodeSymbol glr node =
    ParseTables.getStateSymbol glr.tables node.state


  let incRefCt node =
    node.referenceCount <- node.referenceCount + 1


  let rec decRefCt glr node =
    assert (node.referenceCount > 0);

    node.referenceCount <- node.referenceCount - 1;

    if false then (
      Printf.printf "decrementing node %d to %d\n" node.state node.referenceCount;
      flush stdout;
    );

    if node.referenceCount = 0 then (
      deinit glr node;
      Objpool.dealloc glr.stackNodePool node
    )


  and deinit glr node =
    deallocSemanticValues glr node;

    (* this is implicit in the C++ implementation because firstSib.sib
     * is an RCPtr in C++ *)
    if node.firstSib.sib != cNULL then
      decRefCt glr node.firstSib.sib;

    node.firstSib.sib <- cNULL;

    if Options._accounting () then (
      glr.stats.numStackNodesAllocd <- glr.stats.numStackNodesAllocd - 1;
    )


  and deallocSemanticValues glr node =
    (* explicitly deallocate siblings, so I can deallocate their
     * semantic values if necessary (this requires knowing the
     * associated symbol, which the sibling_links don't know) *)
    if node.firstSib.sib != cNULL then
      User.deallocateSemanticValue glr.userAct (getNodeSymbol glr node) node.firstSib.sval;

    List.iter (fun s ->
      User.deallocateSemanticValue glr.userAct (getNodeSymbol glr node) s.sval;

      (* this is implicit in the C++ version, due to Owner<> *)
      decRefCt glr s.sib
    ) node.leftSiblings;

    node.leftSiblings <- []


  let init node state =
    node.state <- state;
    assert (node.leftSiblings == []);
    assert (node.firstSib.sib == cNULL);
    node.referenceCount <- 0;
    node.determinDepth  <- 1


  let make glr state =
    if Options._accounting () then (
      glr.stats.numStackNodesAllocd <- glr.stats.numStackNodesAllocd + 1;
      if glr.stats.numStackNodesAllocd > glr.stats.maxStackNodesAllocd then
        glr.stats.maxStackNodesAllocd <- glr.stats.numStackNodesAllocd;
    );

    let node = Objpool.alloc glr.stackNodePool in
    init node state;
    node.column <- glr.globalNodeColumn;
    node


  let hasZeroSiblings node =
    node.firstSib.sib == cNULL


  let hasOneSibling node =
    node.firstSib.sib != cNULL && node.leftSiblings == []


  let hasMultipleSiblings node =
    node.leftSiblings != []


  (* add the very first sibling *)
  let addFirstSiblingLink_noRefCt node leftSib sval start_p end_p =
    assert (hasZeroSiblings node);

    (* my depth will be my new sibling's depth, plus 1 *)
    node.determinDepth <- leftSib.determinDepth + 1;

    (* we don't have any siblings yet; use embedded
     * don't update reference count of 'leftSib', instead caller must do so *)
    assert (node.firstSib.sib == cNULL);
    node.firstSib.sib <- leftSib;     (* update w/o refct *)

    node.firstSib.sval <- sval;
    node.firstSib.start_p <- start_p;
    node.firstSib.end_p <- end_p


  (* pulled out of 'addSiblingLink' so I can inline addSiblingLink
   * without excessive object code bloat; the branch represented by
   * the code in this function is much less common *)
  let addAdditionalSiblingLink node leftSib sval start_p end_p =
    (* there's currently at least one sibling, and now we're adding another;
     * right now, no other stack node should point at this one (if it does,
     * most likely will catch that when we use the stale info)
     *
     * now there is a second outgoing pointer *)
    node.determinDepth <- 0;

    (* this was implicit in the C++ verison *)
    incRefCt leftSib;

    let link = SiblingLink.create leftSib sval start_p end_p in
    node.leftSiblings <- link :: node.leftSiblings;

    link


  (* add a new sibling by creating a new link *)
  let addSiblingLink node leftSib sval start_p end_p =
    if node.firstSib.sib == cNULL then (
      addFirstSiblingLink_noRefCt node leftSib sval start_p end_p;

      (* manually increment leftSib's refct *)
      incRefCt leftSib;

      (* pointer to firstSib.. *)
      node.firstSib
    ) else (
      (* as best I can tell, x86 static branch prediction is simply
       * "conditional forward branches are assumed not taken", hence
       * the uncommon case belongs in the 'else' branch *)
      addAdditionalSiblingLink node leftSib sval start_p end_p
    )


  let getUniqueLink node =
    assert (hasOneSibling node);
    node.firstSib


  let getLinkTo node another =
    (* check first.. *)
    if node.firstSib.sib == another then (
      Some node.firstSib
    ) else (
      (* check rest *)
      try
        let link = List.find (fun candidate -> candidate.sib == another) node.leftSiblings in
        Some link
      with Not_found ->
        None
    )


  (* printAllocStats goes here *)

  let computeDeterminDepth node =
    if hasZeroSiblings node then (
      1
    ) else if hasOneSibling node then (
      (* it must be equal to sibling's, plus one *)
      node.firstSib.sib.determinDepth + 1
    ) else (
      assert (hasMultipleSiblings node);
      0
    )


  let checkLocalInvariants node =
    computeDeterminDepth node = node.determinDepth

end


(**********************************************************
 * :: Path queue management
 **********************************************************)

module Path : sig

  type t = path

  val create
    : 'result glr
    -> ParseTables.state_id
    -> int
    -> int
    -> t

  val delete
    : 'result glr
    -> t
    -> unit

  val ensureRhsLen
    : t
    -> int
    -> bool

  val insert_copy
    : 'result glr
    -> t
    -> StackNode.t
    -> unit

  val next
    : t
    -> t option

  val detach_queue
    : 'result glr
    -> t option

end = struct

  type t = path

  (* stackTraceString *)

  let create glr ssi pi rhsLen =
    let p = Objpool.alloc glr.pathPool in
    p.startStateId <- ssi;
    p.prodIndex <- pi;
    p.rhsLen <- rhsLen;
    p

  let delete glr p =
    Objpool.dealloc glr.pathPool p


  (* ensure the arrays have at least the given index *)
  let ensureRhsLen p rhsLen =
    Array.length p.sibLinks >= rhsLen &&
    Array.length p.symbols  >= rhsLen


  let compare_path glr p1 p2 =
    if p1.startColumn > p2.startColumn then (
      (* 'p1' spans fewer tokens, so it goes first *)
      -1
    ) else if p2.startColumn > p1.startColumn then (
      (* same logic *)
      1
    ) else (
      let tables = glr.tables in
      (* equal start columns, compare nonterm ids *)
      let p1NtIndex = ParseTables.getProdInfo_lhsIndex tables p1.prodIndex in
      let p2NtIndex = ParseTables.getProdInfo_lhsIndex tables p2.prodIndex in

      (* check nonterm order *)
      let ord1 = ParseTables.getNontermOrdinal tables p1NtIndex in
      let ord2 = ParseTables.getNontermOrdinal tables p2NtIndex in

      ord1 - ord2
    )


  let rec search_pos glr p prev =
    match prev.next with
    | Some next when compare_path glr p next < 0 ->
        search_pos glr p next
    | _ ->
        prev


  let insert_copy glr src leftEdge =
    let rhsLen = src.rhsLen in

    (* make a new node *)
    let p = create glr src.startStateId src.prodIndex rhsLen in

    (* fill in left edge info *)
    p.leftEdgeNode <- leftEdge;
    p.startColumn  <- leftEdge.column;

    (* copy path info *)
    for i = 0 to rhsLen - 1 do
      p.sibLinks.(i) <- src.sibLinks.(i);
      p.symbols .(i) <- src.symbols .(i);
    done;

    (* find proper place to insert new path *)
    match glr.top with
    | None ->
        (* prepend *)
        p.next <- None;
        glr.top <- Some p;

    | Some top ->
        if compare_path glr p top < 0 then (
          p.next <- glr.top;
          glr.top <- Some p;
        ) else (
          (* search *)
          let prev = search_pos glr p top in

          (* insert *)
          p.next <- prev.next;
          prev.next <- Some p;
        )

        
  let next path =
    path.next


  let detach_queue glr =
    let ret = glr.top in
    glr.top <- None;
    ret

end


(**********************************************************
 * :: Reduction worklist algorithm
 **********************************************************)

module RWL : sig

  val addTopmostParser
    : 'result glr
    -> stack_node
    -> unit

  val enqueueReductions
    : 'result glr
    -> stack_node
    -> ParseTables.action_entry
    -> sibling_link option
    -> int

  val processWorklist
    : 'result glr
    -> ParseTables.term_index
    -> Lexing.position
    -> unit
  
  val shiftTerminals
    : 'result glr
    -> ParseTables.term_index
    -> SemanticValue.t
    -> SourceLocation.t
    -> unit

end = struct

  (* add a new parser to the 'active_parsers' list, maintaining
   * related invariants*)
  let addTopmostParser glr parsr =
    assert (StackNode.checkLocalInvariants parsr);

    Arraystack.push parsr glr.active_parsers;
    StackNode.incRefCt parsr


  (* same argument meanings as for 'recursiveEnqueue' *)
  let rec collectPathLink glr proto popsRemaining currentNode mustUseLink linkToAdd =
    proto.sibLinks.(popsRemaining) <- linkToAdd;
    proto.symbols .(popsRemaining) <- StackNode.getNodeSymbol glr currentNode;

    recursiveEnqueue glr proto popsRemaining linkToAdd.sib (
      match mustUseLink with
      | Some link when link == linkToAdd ->
          None
      | _ ->
          (* consume must-use link *)
          mustUseLink
    )


  (* recursive depth-first enumeration of paths *)
  and recursiveEnqueue glr
    proto         (* prototype path, with path so far *)
    popsRemaining (* # of links yet to traverse to find a full path *)
    currentNode   (* node we're at in the path *)
    mustUseLink   (* link the path must use (if not None) *)
  =
    if popsRemaining = 0 then (
      (* found path *)

      (* must have used the link *)
      match mustUseLink with
      | Some _ ->
          (* do nothing *)
          ()

      | None ->
          (* copy the prototype path, it's the one we want *)
          Path.insert_copy glr proto currentNode

    ) else (

      (* explore currentNode's siblings *)
      collectPathLink glr proto (popsRemaining - 1) currentNode mustUseLink currentNode.firstSib;

      List.iter (fun sibling ->
        collectPathLink glr proto (popsRemaining - 1) currentNode mustUseLink sibling
      ) currentNode.leftSiblings
    )


  let enqueueReduceAction glr parsr action mustUseLink =
    let prodIndex = ParseTables.decodeReduce glr.tables action parsr.state in

    (* production info *)
    let rhsLen = ParseTables.getProdInfo_rhsLen glr.tables prodIndex in
    assert (rhsLen >= 0);       (* paranoia *)

    (* make a prototype path; used to control recursion *)
    let proto = Path.create glr parsr.state prodIndex rhsLen in
    assert (Path.ensureRhsLen proto rhsLen);

    (* kick off the recursion *)
    recursiveEnqueue glr proto rhsLen parsr mustUseLink;

    (* deallocate prototype *)
    Path.delete glr proto


  (* returns # of actions *)
  let rec enqueueReductions glr parsr action mustUseLink =
    assert (StackNode.checkLocalInvariants parsr);

    if ParseTables.isShiftAction glr.tables action then (
      (* do nothing, only looking for reductions *)
      1
    ) else if ParseTables.isReduceAction glr.tables action then (
      enqueueReduceAction glr parsr action mustUseLink;
      1
    ) else if ParseTables.isErrorAction glr.tables action then (
      (* parser just dies *)
      0
    ) else (
      (* ambiguous; check for reductions in list of actions *)
      let firstEntry = ParseTables.decodeAmbigAction glr.tables action parsr.state in
      let numEntries = (ParseTables.getAmbigEntry glr.tables firstEntry :> int) in

      for i = 1 to numEntries do
        let entry = ParseTables.getAmbigEntry glr.tables (firstEntry + i) in
        (* ignore return value because I know it will be 1 *)
        ignore (enqueueReductions glr parsr entry mustUseLink);
      done;

      numEntries
    )


  let findTopmostParser glr state =
    (* always using the *not* USE_PARSER_INDEX case *)
    Arraystack.find (fun n -> n.state = state) glr.active_parsers


  let canMakeProgress glr tokType parsr =
    let entry = ParseTables.getActionEntry glr.tables parsr.state tokType in

    ParseTables.isShiftAction glr.tables entry
      || ParseTables.isReduceAction glr.tables entry
      || not (ParseTables.isErrorAction glr.tables entry)


  let shiftActive glr tokType leftSibling rightSibling lhsIndex sval start_p end_p =
    match StackNode.getLinkTo rightSibling leftSibling with
    | Some sibLink ->
        (* we already have a sibling link, don't need a new one *)

        (* +--------------------------------------------------+
         * | it is here that we are bringing the tops of two  |
         * | alternative parses together (TREEBUILD)          |
         * +--------------------------------------------------+
         *)

        (* dead tree optimisation *)
        if not (canMakeProgress glr tokType rightSibling) then (
          if Options._trace_parse () then
            Printf.printf "avoided a merge by noticing the state was dead\n";
          User.deallocateSemanticValue glr.userAct (StackNode.getNodeSymbol glr rightSibling) sval;
        ) else (
          (* call user's merge code *)
          sibLink.sval <- User.mergeAlternativeParses glr.userAct lhsIndex sibLink.sval sval;
        );

        (* ok, done *)
        None

        (* didn't add a link, no potential for new paths *)

    | None ->
        (* we get here if there is no suitable sibling link already
         * existing; so add the link (and keep the ptr for loop below) *)
        let sibLink = StackNode.addSiblingLink rightSibling leftSibling sval start_p end_p in

        (* adding a new sibling link may have introduced additional
         * opportunities to do reductions from parsers we thought
         * we were finished with.
         *
         * what's more, it's not just the parser ('rightSibling') we
         * added the link to -- if rightSibling's itemSet contains 'A ->
         * alpha . B beta' and B ->* empty (so A's itemSet also has 'B
         * -> .'), then we reduced it (if lookahead ok), so
         * 'rightSibling' now has another left sibling with 'A -> alpha
         * B . beta'.  We need to let this sibling re-try its reductions
         * also.
         *
         * so, the strategy is to let all 'finished' parsers re-try
         * reductions, and process those that actually use the just-
         * added link *)

        (* we don't have to recompute if nothing else points at
         * 'rightSibling'; the refct is always at least 1 because we found
         * it on the "active parsers" worklist *)
        if rightSibling.referenceCount > 1 then (
          (* since we added a new link *all* determinDepths might
           * be compromised; iterating more than once should be very
           * rare (and this code path should already be unusual) *)
          let changes = ref true in
          let iters   = ref 0 in

          while !changes do
            changes := false;
            Arraystack.iter (fun parsr ->
              let newDepth = StackNode.computeDeterminDepth parsr in
              if newDepth <> parsr.determinDepth then (
                changes := true;
                parsr.determinDepth <- newDepth;
              )
            ) glr.active_parsers;
            incr iters;
            assert (!iters < 1000);     (* protect against infinite loop *)
          done
        );

        (* inform the caller that a new sibling link was added *)
        Some sibLink


  let shiftNew glr tokType leftSibling rightSiblingState sval start_p end_p =
    (* not already active parser in this state, so make one *)
    let rightSibling = StackNode.make glr rightSiblingState in

    (* add link *)
    ignore (StackNode.addSiblingLink rightSibling leftSibling sval start_p end_p);

    (* extend frontier *)
    addTopmostParser glr rightSibling;

    (* enqueue this new parser's reductions *)
    let action = ParseTables.getActionEntry glr.tables rightSibling.state tokType in
    ignore (enqueueReductions glr rightSibling action None(*siblink*));

    (* caller doesn't need to do anything more *)
    None


  let shiftNonterminal glr tokType leftSibling lhsIndex sval start_p end_p =
    (* consult goto table to find where to go upon "shifting" the nonterminal *)
    let rightSiblingState =
      ParseTables.getGoto glr.tables leftSibling.state lhsIndex
    in

    if Options._trace_parse () then
      Printf.printf "state %d, %s nonterm %d (%s), to state %d\n"
        (leftSibling.state :> int)
        (Colour.cyan "shift")
        (lhsIndex :> int)
        (User.nonterminalName glr.userAct lhsIndex)
        (rightSiblingState :> int);

    (* is there already an active parser with this state? *)
    match findTopmostParser glr rightSiblingState with
    | Some rightSibling ->
        shiftActive glr tokType leftSibling rightSibling lhsIndex sval start_p end_p

    | None ->
        shiftNew glr tokType leftSibling rightSiblingState sval start_p end_p


  let rec recursiveProcess glr tokType start_p path =
    (* info about the production *)
    let rhsLen   = ParseTables.getProdInfo_rhsLen   glr.tables path.prodIndex in
    let lhsIndex = ParseTables.getProdInfo_lhsIndex glr.tables path.prodIndex in

    if Options._trace_parse () then
      Printf.printf "state %d, %s by production %d (rhsLen=%d), back to state %d\n"
                     (path.startStateId :> int)
                     (Colour.cyan "reduce")
                     path.prodIndex
                     rhsLen
                     (path.leftEdgeNode.state :> int);

    if Options._accounting () then
      glr.stats.nondetReduce <- glr.stats.nondetReduce + 1;

    (* record location of left edge; initially is location of
     * the lookahead token *)
    let leftEdge = ref start_p in
    let rightEdge = ref Lexing.dummy_pos in

    (* before calling the user, duplicate any needed values *)
    for i = rhsLen - 1 downto 0 do
      let sib = path.sibLinks.(i) in

      (* put a copy of the sval in the array that will be passed to the user *)
      glr.toPass.(i) <- User.duplicateSemanticValue glr.userAct path.symbols.(i) sib.sval;
      if Options._trace_parse () then
        Printf.printf "toPass[%d] = %s\n" i (unique_id glr.toPass.(i));

      if sib.start_p != Lexing.dummy_pos then
        leftEdge := sib.start_p;
      if !rightEdge == Lexing.dummy_pos && sib.end_p != Lexing.dummy_pos then
        rightEdge := sib.end_p;
    done;

    (* invoke user's reduction action (TREEBUILD) *)
    begin try
      let sval = User.reductionAction glr.userAct path.prodIndex glr.toPass !leftEdge !rightEdge in
      (* did user want to keep? *)
      if not (User.keepNontermValue glr.userAct lhsIndex sval) then
        keep_cancel ();
      if Options._trace_parse () then
        Printf.printf "result: %s%s %s\n"
          (User.nonterminalName glr.userAct lhsIndex)
          (unique_id sval)
          (User.showNontermValue glr.userAct lhsIndex sval);
     
      (* shift the nonterminal, sval *)
      let newLink = shiftNonterminal glr tokType path.leftEdgeNode lhsIndex sval !leftEdge !rightEdge in

      if newLink != None then
        (* for each 'finished' parser, enqueue actions enabled by the new link *)
        Arraystack.iter (fun parsr ->
          let action = ParseTables.getActionEntry glr.tables parsr.state tokType in
          ignore (enqueueReductions glr parsr action newLink)
        ) glr.active_parsers

    with UserActions.Cancel reason ->
      (* cancelled; drop on floor *)
      ()
    end;

    (* we dequeued it above, and are now done with it, so recycle
     * it for future use *)
    Path.delete glr path;

    match Path.next path with
    | None -> ()
    | Some next -> recursiveProcess glr tokType start_p next


  let rec processWorklist glr tokType start_p =
    match Path.detach_queue glr with
    | None -> ()  (* nothing to do *)

    | Some top ->
        (* process the enabled reductions in priority order *)
        recursiveProcess glr tokType start_p top;
        processWorklist glr tokType start_p


  let findShift tables tokType action state =
    (* consult action table, looking for shifts *)
    if ParseTables.isShiftAction tables action then (
      (* unambiguous shift *)
      ParseTables.decodeShift tables action tokType
    ) else if ParseTables.isReduceAction tables action
           || ParseTables.isErrorAction tables action then (
      (* unambiguous reduction or error *)
      cSTATE_INVALID
    ) else (
      (* nondeterministic *)
      let firstEntry = ParseTables.decodeAmbigAction tables action state in
      let numEntries = (ParseTables.getAmbigEntry tables firstEntry :> int) in

      let newState = ref cSTATE_INVALID in
      let i = ref 1 in
      while !i <> numEntries do
        let action = ParseTables.getAmbigEntry tables (firstEntry + !i) in
        incr i;
        if ParseTables.isShiftAction tables action then (
          (* a shift was among the conflicted actions *)
          newState := ParseTables.decodeShift tables action tokType;

          (* "break" *)
          i := numEntries
        )
      done;

      !newState
    )


  let shiftTerminals glr tokType tokSval tokSloc =
    glr.globalNodeColumn <- glr.globalNodeColumn + 1;

    (* move all parsers from 'active_parsers' to 'prev_active' *)
    assert (Arraystack.is_empty glr.prev_active);
    Arraystack.swap glr.prev_active glr.active_parsers;
    assert (Arraystack.is_empty glr.active_parsers);

    (* for token multi-yield.. *)
    let prev = ref SiblingLink.cNULL in

    Arraystack.iter (fun leftSibling ->
      (* take the node from 'prev_active'; the refcount transfers
       * from 'prev_active' to (local variable) 'leftSibling' *)
      assert (leftSibling.referenceCount >= 1);   (* for the local *)
      let state = leftSibling.state in

      (* can this parser shift? *)
      let action = ParseTables.getActionEntry glr.tables state tokType in

      (* if we find a shift, this will be set to something valid *)
      let newState = findShift glr.tables tokType action state in

      if newState <> cSTATE_INVALID then (
        (* found a shift *)

        if Options._accounting () then
          glr.stats.nondetShift <- glr.stats.nondetShift + 1;

        if Options._trace_parse () then
          Printf.printf "state %d, %s token %s, to state %d\n"
                         (state :> int)
                         (Colour.cyan "shift")
                         (User.terminalName glr.userAct tokType)
                         (newState :> int);

        (* already a parser in this state? *)
        let rightSibling =
          match findTopmostParser glr newState with
          | Some rs ->
              (* use existing *)
              rs
          | None ->
              (* must make a new stack node *)
              let rs = StackNode.make glr newState in
              (* add it to active parsers *)
              addTopmostParser glr rs;
              (* use new *)
              rs
        in

        (* semantic value for this token *)
        prev :=
          if !prev == SiblingLink.cNULL then (
              (* usual case *)
              StackNode.addSiblingLink rightSibling leftSibling
                tokSval (fst tokSloc) (snd tokSloc)

          ) else (
              (* the 'sval' we just grabbed has already been claimed by
               * 'prev.sval'; get a fresh one by duplicating the latter *)
              let sval = User.duplicateTerminalValue glr.userAct tokType !prev.sval in

              (* add sibling link now *)
              StackNode.addSiblingLink rightSibling leftSibling
                sval !prev.start_p !prev.end_p
          );

        (* adding this sibling link cannot violate the determinDepth
         * invariant of some other node, because all of the nodes created
         * or added-to during shifting do not have anything pointing at
         * them, so in particular nothing points to 'rightSibling'; a simple
         * check of this is to check the reference count and verify it is 1,
         * the 1 being for the 'active_parsers' list it is on *)
        assert (rightSibling.referenceCount = 1);
      );

      (* pending decrement of leftSibling, which is about to go out of scope *)
      StackNode.decRefCt glr leftSibling;
    ) glr.prev_active;

    Arraystack.clear glr.prev_active

end


(**********************************************************
 * :: Common to GLR and LR Parser cores
 **********************************************************)

module type ParserCore = sig

  val parseToken
    : 'result glr
    -> ParseTables.term_index
    -> SemanticValue.t
    -> SourceLocation.t
    -> unit

end

let parse_error ?reason glr tokType tokSloc lastToDie =
  if Options._error_verbose () then (
    if lastToDie <> cSTATE_INVALID then (
      Printf.printf "In state %d, I expected one of these tokens:\n"
        (lastToDie :> int);
      ParseTables.iter_terms glr.tables (fun i ->
        let act = ParseTables.getActionEntry glr.tables lastToDie i in
        if not (ParseTables.isErrorAction glr.tables act) then
          Printf.printf "  [%d] %s\n" (i :> int) (User.terminalName glr.userAct i);
      )
    ) else (
      Printf.printf "(expected-token info not available due to nondeterministic mode)\n"
    );

    Printf.printf (*loc*) "Parse error (state %d) at %s\n"
                  (lastToDie :> int)
                  (User.terminalName glr.userAct tokType);
    match reason with
    | None -> ()
    | Some reason ->
        Printf.printf "Last reduction was cancelled because: %s\n" reason
  );

  raise (Located (tokSloc, ParseError (lastToDie, tokType), User.terminalName glr.userAct tokType))


(**********************************************************
 * :: Non-deterministic parser core
 **********************************************************)

module GLR : ParserCore = struct

  let nondeterministicParseToken glr tokType tokSval tokSloc =
    let lastToDie = ref cSTATE_INVALID in

    (* seed the reduction worklist by analysing the top nodes *)
    Arraystack.iter (fun parsr ->
      let action = ParseTables.getActionEntry glr.tables parsr.state tokType in
      let actions = RWL.enqueueReductions glr parsr action None(*sibLink*) in

      if actions = 0 then (
        if Options._trace_parse () then
          Printf.printf "parser in state %d died\n"
            (parsr.state :> int);
        lastToDie := parsr.state
      )
    ) glr.active_parsers;

    (* drop into worklist processing loop *)
    RWL.processWorklist glr tokType (fst tokSloc);

    (* do all shifts last *)
    RWL.shiftTerminals glr tokType tokSval tokSloc;

    (* error? *)
    if Arraystack.is_empty glr.active_parsers then
      parse_error glr tokType tokSloc !lastToDie


  (* pulled out so I can use this block of statements in several places *)
  let parseToken glr tokType tokSval tokSloc =
    let open Lexerint in

    (* raises ParseError on failure *)
    nondeterministicParseToken glr tokType tokSval tokSloc;

    (* goto label: getNextToken *)
    (* last token? *)
    if tokType = ParseTables.cTERM_EOF then
      raise End_of_file       (* "break" *)

end


(**********************************************************
 * :: Mini-LR core
 **********************************************************)

module LR : ParserCore = struct

  let rec parseToken glr tokType tokSval tokSloc =
    let parsr = ref (Arraystack.top glr.active_parsers) in
    assert (!parsr.referenceCount = 1);

    let action = ParseTables.getActionEntry_noError glr.tables !parsr.state tokType in

    if ParseTables.isReduceAction glr.tables action then (
      (* can reduce unambiguously *)
      let prodIndex = ParseTables.decodeReduce glr.tables action !parsr.state in
      if Options._accounting () then
        glr.stats.detReduce <- glr.stats.detReduce + 1;

      let rhsLen = ParseTables.getProdInfo_rhsLen glr.tables prodIndex in

      if rhsLen <= !parsr.determinDepth then (
        let lhsIndex = ParseTables.getProdInfo_lhsIndex glr.tables prodIndex in

        let startStateId = !parsr.state in

        let leftEdge = ref (fst tokSloc) in
        let rightEdge = ref Lexing.dummy_pos in

        assert (rhsLen <= Array.length glr.toPass);

        (* --- loop for arbitrary rhsLen ---
         * pop off 'rhsLen' stack nodes, collecting as many semantic
         * values into 'toPass'
         * NOTE: this loop is the innermost inner loop of the entire
         * parser engine -- even *one* branch inside the loop body
         * costs about 30% end-to-end performance loss! *)
        for i = rhsLen - 1 downto 0 do
          (* grab the (only) sibling of 'parsr' *)
          let sib = !parsr.firstSib in

          (* Store its semantic value it into array that will be
           * passed to user's routine.  Note that there is no need to
           * dup() this value, since it will never be passed to
           * another action routine (avoiding that overhead is
           * another advantage to the LR mode). *)
          glr.toPass.(i) <- sib.sval;
          if Options._trace_parse () then
            Printf.printf "toPass[%d] = %s\n" i (unique_id glr.toPass.(i));

          (* if it has a valid source location, grab it *)
          if sib.start_p != Lexing.dummy_pos then
            leftEdge := sib.start_p;
          if !rightEdge == Lexing.dummy_pos && sib.end_p != Lexing.dummy_pos then
            rightEdge := sib.end_p;

          (* pop 'parsr' and move to next one *)
          Objpool.dealloc glr.stackNodePool !parsr;
          let prev = !parsr in
          parsr := sib.sib;

          assert (!parsr.referenceCount = 1);
          assert (prev.referenceCount = 1);

          (* adjust a couple things about 'prev' reflecting
           * that it has been deallocated *)
          if Options._accounting () then (
            glr.stats.numStackNodesAllocd <- glr.stats.numStackNodesAllocd - 1;
          );
          prev.firstSib.sib <- StackNode.cNULL;

          assert (!parsr.referenceCount = 1);
        done;

        (* now, do an abbreviated 'glrShiftNonterminal' *)
        let newState = ParseTables.getGoto glr.tables !parsr.state lhsIndex in

        if Options._trace_parse () then (
          Printf.printf "state %d, (unambig) %s by %d (len=%d), back to %d then out to %d\n"
                        (startStateId :> int)
                        (Colour.cyan "reduce")
                        prodIndex
                        rhsLen
                        (!parsr.state :> int)
                        (newState :> int);
          flush stdout;
        );

        (* call the user's action function (TREEBUILD) *)
        let sval =
          try
            let sval = User.reductionAction glr.userAct prodIndex glr.toPass !leftEdge !rightEdge in
            (* does the user want to keep it? *)
            if not (User.keepNontermValue glr.userAct lhsIndex sval) then
              keep_cancel ();
            if Options._trace_parse () then
              Printf.printf "result: %s %s\n"
                (unique_id sval)
                (User.showNontermValue glr.userAct lhsIndex sval);

            sval
          with UserActions.Cancel reason ->
            parse_error ~reason glr tokType tokSloc newState
        in

        (* the sole reference is the 'parsr' variable *)
        assert (!parsr.referenceCount = 1);

        (* push new state *)
        let newNode = StackNode.make glr newState in

        StackNode.addFirstSiblingLink_noRefCt newNode !parsr sval !leftEdge !rightEdge;

        assert (!parsr.referenceCount = 1);

        (* replace old topmost parser with 'newNode' *)
        assert (Arraystack.length glr.active_parsers = 1);
        Arraystack.set glr.active_parsers 0 newNode;
        StackNode.incRefCt newNode;
        assert (newNode.referenceCount = 1);

        (* we have not shifted a token, so again try to use
         * the deterministic core *)
        parseToken glr tokType tokSval tokSloc
      ) else (
        (* deterministic depth insufficient: use GLR *)
        GLR.parseToken glr tokType tokSval tokSloc
      )

    ) else if ParseTables.isShiftAction glr.tables action then (
      (* can shift unambiguously *)
      let newState = ParseTables.decodeShift glr.tables action tokType in
      if Options._accounting () then
        glr.stats.detShift <- glr.stats.detShift + 1;

      if Options._trace_parse () then (
        Printf.printf "state %d, (unambig) %s token %d, to state %d\n"
                      (!parsr.state :> int)
                      (Colour.cyan "shift")
                      (tokType :> int)
                      (newState :> int);
        flush stdout;
      );

      glr.globalNodeColumn <- glr.globalNodeColumn + 1;

      let rightSibling = StackNode.make glr newState in

      StackNode.addFirstSiblingLink_noRefCt rightSibling !parsr tokSval (fst tokSloc) (snd tokSloc);

      (* replace 'parsr' with 'rightSibling' *)
      assert (Arraystack.length glr.active_parsers = 1);
      Arraystack.set glr.active_parsers 0 rightSibling;

      assert (!parsr.referenceCount = 1);
      assert (rightSibling.referenceCount = 0);

      rightSibling.referenceCount <- 1;

      (* get next token *)
      (* "goto getNextToken;" *)
      (* last token? *)
      if tokType = ParseTables.cTERM_EOF then
        raise End_of_file       (* "break" *)

    ) else (
      (* error or ambig; not deterministic *)
      GLR.parseToken glr tokType tokSval tokSloc
    )

end


(**********************************************************
 * :: Debugging and tracing
 **********************************************************)

let nodeSummary node =
  Printf.sprintf "%d[%d]" (node.state :> int) node.referenceCount


let rec innerStackSummary printed node =
  if List.memq node !printed then (

    (* already printed *)
    "(rep:" ^ nodeSummary node ^ ")"

  ) else (

    (* remember that we've now printed 'node' *)
    printed := node :: !printed;

    if node.firstSib.sib == StackNode.cNULL then (
      (* no siblings *)
      nodeSummary node

    ) else if node.leftSiblings == [] then (
      (* one sibling *)
      nodeSummary node ^ "-" ^
      innerStackSummary printed node.firstSib.sib

    ) else (
      (* multiple siblings *)
      (* force order of eval *)
      let nodeSummary = nodeSummary node in
      let firstSummary = innerStackSummary printed node.firstSib.sib in
      let siblingsSummary =
        List.fold_left (fun acc link ->
          acc ^ "|" ^ innerStackSummary printed link.sib
        ) "" node.leftSiblings
      in
      Printf.sprintf "%s-(%s%s)" nodeSummary firstSummary siblingsSummary
    )
  )


let stackSummary glr =
  (* nodes already printed *)
  let printed = ref [] in

  (* loop/fold *)
  let len = Arraystack.length glr.active_parsers in
  let rec loop acc i =
    if i > len - 1 then
      (* done *)
      acc
    else
      let n = Arraystack.nth glr.active_parsers i in
      let summary = Printf.sprintf "%s (%d: %s)"
        acc i (innerStackSummary printed n)
      in

      loop summary (i + 1)
  in

  loop "" 0


(**********************************************************
 * :: Main parser loop
 **********************************************************)

(* This function is the core of the parser, and its performance is
 * critical to the end-to-end performance of the whole system.
 * It does not actually return, but it has the same return type as
 * the main entry point. *)
let rec main_loop (glr : 'result glr) lexer token : 'result =
  if Options._trace_parse () then (
    let open Lexerint in
    let tokType = lexer.index token in

    Printf.printf "---- processing token %s, %d active parsers ----\n"
                   (User.terminalName glr.userAct tokType)
                   (Arraystack.length glr.active_parsers);
    Printf.printf "Stack:%s\n" (stackSummary glr);
    flush stdout
  );

  (* classify and decompose current token *)
  let tokType, tokSval, tokSloc = User.reclassifyToken glr.userAct lexer token in

  begin try
    if Options._use_mini_lr () && Arraystack.length glr.active_parsers = 1 then
      (* try deterministic parsing *)
      LR.parseToken glr tokType tokSval tokSloc
    else
      (* mini lr core disabled, use full GLR *)
      GLR.parseToken glr tokType tokSval tokSloc;
  with
  | Located _
  | End_of_file as e ->
      (* propagate internal exceptions and ones
       * already wrapped in Located *)
      raise e
  | e ->
      raise (Located (tokSloc, e, Printexc.get_backtrace ()))
  end;

  (* parse next token *)
  main_loop glr lexer Lexerint.(lexer.token ())


(**********************************************************
 * :: Entry and exit of GLR parser
 **********************************************************)

(* used to extract the svals from the nodes just under the
 * start symbol reduction *)
let grabTopSval glr node =
  let sib = StackNode.getUniqueLink node in
  let ret = sib.sval in
  sib.sval <- User.duplicateSemanticValue glr.userAct (StackNode.getNodeSymbol glr node) sib.sval;

  (* TRSACTION("dup'd " << ret << " for top sval, yielded " << sib->sval); *)

  ret


let cleanupAfterParse (glr : 'result glr) : 'result =
  if Options._trace_parse () then
    Printf.printf "==== parse succeeded ====\n";

  if not (Arraystack.length glr.active_parsers = 1) then (
    Printf.printf "parsing finished with %d active parsers!\n"
                  (Arraystack.length glr.active_parsers);
    raise (ParseError (cSTATE_INVALID, ParseTables.cTERM_INVALID))
  ) else (
    let last = Arraystack.top glr.active_parsers in

    (* prepare to run final action *)
    let arr =
      Array.init 2 (function
        | 0 ->
            let nextToLast = (StackNode.getUniqueLink last).sib in
            (* sval we want *)
            grabTopSval glr nextToLast
        | 1 ->
            (* EOF's sval *)
            grabTopSval glr last
        | _ -> assert false
      )
    in

    (* reduce *)
    let finalProductionIndex = ParseTables.getFinalProductionIndex glr.tables in
    let treeTop =
      SemanticValue.obj (
        User.reductionAction glr.userAct finalProductionIndex arr
          Lexing.dummy_pos Lexing.dummy_pos
      )
    in

    (* before pool goes away.. *)
    Arraystack.iter (StackNode.decRefCt glr) glr.active_parsers;

    treeTop
  )


let parse (glr : 'result glr) lexer : 'result =
  unique_table := empty_unique_table;

  if glr.globalNodeColumn <> 0 then
    failwith "cannot reuse glr object for multiple parses";

  begin
    let startState = ParseTables.getStartState glr.tables in
    let first = StackNode.make glr startState in
    RWL.addTopmostParser glr first;
  end;

  if Options._trace_parse () then
    Printf.printf "==== starting parse ====\n";

  (* main parsing loop; this function never returns normally *)
  try

    (* get first token and start parsing *)
    main_loop glr lexer Lexerint.(lexer.token ())

  with End_of_file ->

    (* end of parse *)
    cleanupAfterParse glr


(**********************************************************
 * :: Creating the GLR parser object
 **********************************************************)

let makePath maxRhsLen () = {
  sibLinks     = Array.make maxRhsLen SiblingLink.cNULL;
  symbols      = Array.make maxRhsLen ParseTables.cSYMBOL_INVALID;
  startStateId = cSTATE_INVALID;
  prodIndex    = -1;
  rhsLen       = -1;
  startColumn  = -1;
  leftEdgeNode = StackNode.cNULL;
  next         = None;
}


let computeMaxRhsLen tables =
  CoreInt.fold_left (fun len i ->
    max len (ParseTables.getProdInfo_rhsLen tables i)
  ) 0 0 (ParseTables.getNumProds tables - 1)


let create userAct tables =
  let maxRhsLen = computeMaxRhsLen tables in

  {
    userAct;
    tables;
    top                 = None;
    toPass              = Array.make maxRhsLen SemanticValue.null;
    pathPool		= Objpool.make (makePath maxRhsLen);
    active_parsers      = Arraystack.create ();
    prev_active         = Arraystack.create ();
    stackNodePool       = Objpool.make StackNode.create;
    globalNodeColumn    = 0;
    stats = {
      numStackNodesAllocd = 0;
      maxStackNodesAllocd = 0;
      detShift            = 0;
      detReduce           = 0;
      nondetShift         = 0;
      nondetReduce        = 0;
    };
  }
