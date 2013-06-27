open AnalysisEnvType


let name_of_symbol_opt terms nonterms = function
  | Some sym -> Sloc.value (GrammarUtil.name_of_symbol terms nonterms sym)
  | None -> "None"


let ordering_operator order =
  if order < 0 then
    "<"
  else if order > 0 then
    ">"
  else
    "="


let compare_by_outgoing syms foldl_untili get transition a b =
  foldl_untili (fun t _ ->
    let dest_a = get (transition a) t in
    let dest_b = get (transition b) t in

    match dest_a, dest_b with
    | None, Some _ -> 1
    | Some _, None -> -1
    | Some dest_a, Some dest_b ->
        Ids.State.compare dest_a.state_id dest_b.state_id
    | _ -> 0
  ) 0 syms


let compare_by_reductions index a b =
  let open GrammarType in

  TermArray.foldl_until (fun term ->
    let red_a = List.sort compare (ItemSet.possible_reductions index a term) in
    let red_b = List.sort compare (ItemSet.possible_reductions index b term) in

    compare red_a red_b
  ) 0 index.terms


let renumber_states_compare env a b =
  let open GrammarType in

  (* order them first by their incoming arc symbol; this affects
   * the renumbering that the Code Reduction Scheme demands *)
  let order = GrammarUtil.compare_symbol a.state_symbol b.state_symbol in

  (* from this point on, the CRS would be happy with an arbitrary
   * order, but I want the state numbering to be canonical so that
   * I have an easier time debugging and comparing parse traces
   *
   * they have the same incoming arc symbol; now, sort by outgoing
   * arc symbols
   *)

  let (|<>) a b = if a != 0 then a else Lazy.force b in

  let arbitrary_order = a != b && order == 0 in

  let order =
    order
    (* first up: terminals *)
    |<> lazy (compare_by_outgoing env.index.terms TermArray.foldl_untili TermArray.get (fun is -> is.term_transition) a b)
    (* next: nonterminals *)
    |<> lazy (compare_by_outgoing env.index.nonterms NtArray.foldl_untili NtArray.get (fun is -> is.nonterm_transition) a b)
    (* finally, order by possible reductions *)
    |<> lazy (compare_by_reductions env.index a b)
  in

  if Options._trace_renumbering () then (
    if arbitrary_order then (
      Printf.printf "%a[%s] %s %a[%s]\n"
        Ids.State.print a.state_id
        (name_of_symbol_opt env.index.terms env.index.nonterms a.state_symbol)
        (ordering_operator order)
        Ids.State.print b.state_id
        (name_of_symbol_opt env.index.terms env.index.nonterms b.state_symbol);
      PrintAnalysisEnv.print_item_set env stdout a;
      PrintAnalysisEnv.print_item_set env stdout b;
    );
  );

  (* validate invariants *)
  if a != b then (
    assert (order != 0);
    if Ids.State.is_start a.state_id then
      assert (order < 0);
    if Ids.State.is_start b.state_id then
      assert (order > 0);
  ) else (
    assert (order == 0);
  );

  order


(* The purpose of this function is to number the states (which have up
 * to this point been numbered arbitrarily) in such a way that all
 * states that have a given symbol on incoming arcs will be numbered
 * consecutively.  This is part of the table compression schemes
 * described in the Dencker et. al. paper (see module Parsetables). *)
let renumber_states env states =
  (* sort them in the right order *)
  let states = List.sort (renumber_states_compare env) states in

  (* number them in that order *)
  BatList.iteri (fun i state ->
    if i == 0 then (
      (* the first element should always be the start state *)
      assert (Ids.State.is_start state.state_id);
      assert (BatOption.get env.start_state == state);
    );

    state.state_id <- Ids.State.of_int i;
  ) states;

  if false then (
    List.iter (fun state ->
      PrintAnalysisEnv.print_item_set env stdout state;
    ) states;
  );

  states
