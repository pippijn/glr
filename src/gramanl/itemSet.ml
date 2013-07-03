open AnalysisEnvType

(************************************************************
 * :: Structure defining operations
 ************************************************************)

module M : GrammarSig.FullType with type t = item_set = struct

  type t = item_set

  let hash a =
    ItemList.M.hash a.kernel_items

  let compare a b =
    (* since nonkernel items are entirely determined by kernel
     * items, and kernel items are sorted, it's sufficient to
     * check for kernel list equality *)
    ItemList.M.compare a.kernel_items b.kernel_items

  let equal a b =
    compare a b == 0

  let stats _ = failwith "Not supported"
  let reset _ = failwith "Not supported"

  let sexp_of_t = sexp_of_item_set
  let t_of_sexp = item_set_of_sexp

  let default = {
    kernel_items = ItemList.M.default;
    nonkernel_items = [];
    term_transition = TermArray.empty;
    nonterm_transition = NtArray.empty;
    dots_at_end = [];
    state_symbol = None;
    state_id = Ids.State.default;
    bfs_parent = None;
  }

end

module Table = Hashtbl.Make(M)
module Map   = SexpMap.Make(M)
module Set   = SexpSet.Make(M)
module Stack = HashStack.Make(Table)
module Graph = Graph.Persistent.Digraph.ConcreteLabeled(M)(M)


(************************************************************
 * :: Transition functions
 ************************************************************)

let transition_for_term item_set term =
  TermArray.get item_set.term_transition term

let transition_for_nonterm item_set nonterm =
  NtArray.get item_set.nonterm_transition nonterm

let transition item_set sym =
  let open GrammarType in
  match sym with
  | Terminal    (_,    term) -> transition_for_term    item_set    term
  | Nonterminal (_, nonterm) -> transition_for_nonterm item_set nonterm


let set_transition_for_term from_set term to_set =
  TermArray.set from_set.term_transition term (Some to_set)

let set_transition_for_nonterm from_set nonterm to_set =
  NtArray.set from_set.nonterm_transition nonterm (Some to_set)

let set_transition from_set sym to_set =
  let open GrammarType in
  match sym with
  | Terminal    (_,    term) -> set_transition_for_term    from_set    term to_set
  | Nonterminal (_, nonterm) -> set_transition_for_nonterm from_set nonterm to_set


(************************************************************
 * :: Check whether shift on term extends over nonterm
 ************************************************************)

let has_extending_shift prods nonterms item_set nonterm term =
  CoreList.exists_many (fun item ->
    LrItem.is_extending_shift prods nonterms item nonterm term
  ) [item_set.kernel_items.items; item_set.nonkernel_items]


(************************************************************
 * :: Yield possible reductions on terminal 'lookahead'
 ************************************************************)

let possible_reductions index item_set lookahead =
  let open GrammarType in
  List.fold_left (fun reductions item ->
    if Options._use_LR0 () then (
      (* don't check the lookahead *)
      reductions

    ) else if Options._use_SLR1 () then (
      (* the follow of its LHS must include 'lookahead' *)
      let prod = ProdArray.get index.prods item.dprod.prod in
      let left = NtArray.get index.nonterms prod.left in
      if TerminalSet.mem lookahead.tbase.index_id left.follow then
        prod :: reductions
      else (
        if Options._trace_reductions () then (
          Printf.printf "state %a, not reducing by %a because %a is not in follow of %a\n"
            Ids.State.print item_set.state_id
            (PrintGrammar.print_production index.terms index.nonterms) prod
            Sloc.print_string lookahead.tbase.name
            Sloc.print_string left.nbase.name;
        );
        reductions
      )

    ) else if Options._use_LALR1 () || Options._use_LR1 () then (
      (* the item's lookahead must include 'lookahead' *)
      let prod = ProdArray.get index.prods item.dprod.prod in
      if TerminalSet.mem lookahead.tbase.index_id item.lookahead then (
        if Options._trace_reductions () then (
          Printf.printf "state %a, reducing by %a because %a is in lookahead\n"
            Ids.State.print item_set.state_id
            (PrintGrammar.print_production index.terms index.nonterms) prod
            Sloc.print_string lookahead.tbase.name;
        );
        prod :: reductions
      ) else (
        if Options._trace_reductions () then (
          Printf.printf "state %a, not reducing by %a because %a is not in lookahead\n"
            Ids.State.print item_set.state_id
            (PrintGrammar.print_production index.terms index.nonterms) prod
            Sloc.print_string lookahead.tbase.name;
        );
        reductions
      )

    ) else (
      failwith "no LR variant specified"
    )
  ) [] item_set.dots_at_end


(************************************************************
 * :: Inverse transition functions
 ************************************************************)

let eq_option a b =
  match b with
  | None -> false
  | Some b -> a == b

(* the inverse of transition: map a target state to the symbol that
 * would transition to that state (from the given source state) *)
let inverse_transition last_term last_nonterm source target =
  let open GrammarType in
  try
    Terminal (None,
      Ids.Terminal.find (fun term_index ->
        eq_option target (transition_for_term source term_index)
      ) last_term
    )
  with Not_found ->
    Nonterminal (None,
      Ids.Nonterminal.find (fun nt_index ->
        eq_option target (transition_for_nonterm source nt_index)
      ) last_nonterm
    )


(************************************************************
 * :: Compute transition graph over states
 ************************************************************)

let compute_graph states =
  List.fold_left (fun g state ->
    NtArray.fold_left (fun g -> function
      | None -> g
      | Some target ->
          Graph.add_edge g state target
    ) g state.nonterm_transition
  ) Graph.empty states
