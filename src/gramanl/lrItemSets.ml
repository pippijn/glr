open CorePervasives
open AnalysisEnvType


type env = {
  env : AnalysisEnvType.env;
  nonterm_count : int;
  term_count    : int;

  mutable next_item_set_id : int;

  item_sets_pending : item_set ItemList.Stack.t;
  item_sets_done    : item_set ItemList.Table.t;
}


let make_item_set env kernel_items =
  let state_id = Ids.State.of_int env.next_item_set_id in
  env.next_item_set_id <- env.next_item_set_id + 1;

  { ItemSet.M.default with
    kernel_items = { items = kernel_items; hash = 0; };
    term_transition = TermArray.make env.term_count None;
    nonterm_transition = NtArray.make env.nonterm_count None;
    state_id = state_id;
  }


let changed_items prods item_set =
  (* recompute dots_at_end *)
  item_set.dots_at_end <-
    CoreList.fold_left_many (fun dots_at_end item ->
      if LrItem.is_dot_at_end item then
        (* dot is at end *)
        item :: dots_at_end
      else
        (* dot is not at end *)
        dots_at_end
    ) [] [item_set.kernel_items.items; item_set.nonkernel_items];

  (* compute this so we can throw away items later if we want to *)
  item_set.state_symbol <-
    try
      (* need only check kernel items since all nonkernel items
       * have their dots at the left side *)
      let item =
        List.find (fun item ->
          item.dprod.dot != 0
        ) item_set.kernel_items.items
      in
      Some (DottedProduction.symbol_before_dot prods item.dprod)
    with Not_found ->
      None


let production_closure env finished item b worklist prod_index =
  let open GrammarType in

  let prod = ProdArray.get env.index.prods prod_index in

  if Options._trace_closure () then (
    Printf.printf "    considering production %a\n"
      (PrintGrammar.print_production env.index.terms env.index.nonterms) prod;
  );

  (* key to good performance: do *no* dynamic allocation in this
   * loop (one of two inner loops in the grammar analysis), until a
   * new item is actually *needed* (which is the uncommon case) *)

  (* invariant of the indexed productions list *)
  assert (prod.left == b);

  (* construct "B -> . gamma, First(beta LA)";
   * except, don't actually build it until later; in the meantime,
   * determine which DP and lookahead it would use if created *)
  let new_dp = DottedProduction.get env.dotted_prods prod.pbase.index_id 0 (* dot at left *) in

  (* get beta (what follows B in 'item') *)
  let beta = DottedProduction.next env.dotted_prods item.dprod in

  (* get First(beta) -> new item's lookahead *)
  let new_item_la = beta.first_set in

  (* if beta ->* epsilon, add LA *)
  let new_item_la =
    if beta.can_derive_empty then (
      if Options._trace_closure () then (
        Printf.printf "      beta: %a\n"
          (PrintAnalysisEnv.print_dotted_production env.index) beta;
        print_endline "      beta can derive empty";
      );
      TerminalSet.union new_item_la item.lookahead
    ) else (
      if Options._trace_closure () then (
        print_endline "      beta can NOT derive empty";
      );
      new_item_la
    )
  in

  (* except we do not want to put terminals in the lookahead set
   * for which 'prod' is not allowed to reduce when they are next *)
  let new_item_la =
    TerminalSet.diff new_item_la prod.forbid
  in

  if Options._trace_closure () then (
    Printf.printf "      built item %a\n"
      (PrintAnalysisEnv.print_lr_item env) { dprod = new_dp; lookahead = new_item_la };
  );

  (* is 'newDP' already there?
   * check in working and finished tables *)
  let in_finished, already =
    match new_dp.back_pointer with
    | Some _ as item ->
        false, item
    | None ->
        let item =
          try
            Some (DottedProduction.Table.find finished new_dp)
          with Not_found ->
            None
        in
        true, item
  in

  match already with
  | Some already ->
      (* yes, it's already there *)
      if Options._trace_closure () then (
        Printf.printf "      looks similar to %a\n"
          (PrintAnalysisEnv.print_lr_item env) already;
      );

      (* but the new item may have additional lookahead
       * components, so merge them with the old *)
      let merged = TerminalSet.union already.lookahead new_item_la in
      if not (TerminalSet.equal already.lookahead merged) then (
        already.lookahead <- merged;

        if Options._trace_closure () then (
          Printf.printf "      (chg) merged it to make %a\n"
            (PrintAnalysisEnv.print_lr_item env) already;
        );

        (* merging changed 'already' *)
        if in_finished then (
          (* pull from the 'done' list and put in worklist, since the
           * lookahead changed *)
          DottedProduction.Table.remove finished already.dprod;
          assert (already.dprod.back_pointer == None); (* was not on the worklist *)
          already.dprod.back_pointer <- Some already; (* now is on worklist *)
          already :: worklist

        ) else (
          (* 'already' is in the worklist, so that's fine *)
          worklist
        )
      ) else (
        if Options._trace_closure () then (
          print_endline "      the dprod already existed";
        );
        worklist
      )

  | None ->
      (* it's not already there, so add it to worklist (but first
       * actually create it!) *)
      let new_item = {
        dprod = new_dp;
        lookahead = new_item_la;
      } in

      if Options._trace_closure () then (
        print_endline "      this dprod is new, queueing it to add";
      );

      assert (new_item.dprod.back_pointer == None);
      new_item.dprod.back_pointer <- Some new_item;
      new_item :: worklist


let single_item_closure env finished worklist item =
  let open GrammarType in
  let open AnalysisEnvType in
  (* in comments that follow, 'item' is broken down as
   *   A -> alpha . B beta, LA *)

  if Options._trace_closure () then (
    print_string "%%% considering item ";
    PrintAnalysisEnv.print_lr_item env stdout item;
    print_newline ();
  );

  (* get the symbol B (the one right after the dot) *)
  match LrItem.symbol_after_dot item with
  | None ->
      (* dot is at the end *)
      if Options._trace_closure () then (
        print_endline "    dot is at the end"
      );

      worklist

  | Some (Terminal _) ->
      (* symbol after the dot is a terminal *)
      if Options._trace_closure () then (
        print_endline "    symbol after the dot is a terminal"
      );

      worklist

  | Some (Nonterminal (_, b)) ->
      (* for each production "B -> gamma" *)
      List.fold_left
        (production_closure env finished item b)
        worklist
        (NtArray.get env.prods_by_lhs b)


(* based on [ASU] figure 4.33, p.223
 * NOTE: sometimes this is called with nonempty nonkernel items... *)
let item_set_closure env item_set =
  let open GrammarType in

  (* every 'item' on the worklist has item.dprod.back_pointer = Some item;
   * every 'dprod' not associated has dprod.back_pointer = None *)
  let worklist = [] in

  if Options._trace_closure () then (
    print_string "%%% computing closure of ";
    PrintAnalysisEnv.print_item_set env stdout item_set;
  );

  (* set of items we've finished *)
  let finished = DottedProduction.Table.create 13 in

  (* put all the nonkernels we have into 'finished' *)
  List.iter (fun item ->
    DottedProduction.Table.add finished item.dprod item
  ) item_set.nonkernel_items;

  (* clear the non-kernel item list *)
  item_set.nonkernel_items <- [];

  (* first, close the kernel items -> worklist *)
  let worklist =
    List.fold_left
      (single_item_closure env finished)
      worklist item_set.kernel_items.items
  in

  let rec inner_item_set_closure = function
    | item :: worklist ->
        (* pull the first production *)
        assert (DottedProduction.back_pointer item.dprod == item); (* was on worklist *)
        item.dprod.back_pointer <- None; (* now off worklist *)

        (* put it into list of 'done' items; this way, if this
         * exact item is generated during closure, it will be
         * seen and re-inserted (instead of duplicated) *)
        DottedProduction.Table.add finished item.dprod item;

        (* close it -> worklist *)
        let worklist =
          single_item_closure env finished worklist item
        in

        (* work on the new worklist *)
        inner_item_set_closure worklist

    | [] ->
        ()
  in

  inner_item_set_closure worklist;

  (* move everything from 'finished' to the nonkernel items list *)
  item_set.nonkernel_items <-
    DottedProduction.Table.fold (fun dprod item items ->
      item :: items
    ) finished [];

  (* we potentially added a bunch of things *)
  changed_items env.index.prods item_set;

  if Options._trace_closure () then (
    print_string "%%% done with closure of ";
    PrintAnalysisEnv.print_item_set env stdout item_set;
  )


(* yield a new kernel item list by moving the dot across the productions
 * in 'source' that have 'symbol' to the right of the dot; do *not*
 * compute the closure *)
let move_dot_no_closure dotted_prods source symbol =
  let kernel_items =
    CoreList.fold_left_many (fun kernel_items item ->
      match LrItem.symbol_after_dot item with
      (* dot is already at end *)
      | None -> kernel_items
      (* can't move dot *)
      | Some sym when not (GrammarUtil.equal_symbol sym symbol) -> kernel_items

      | Some _ ->
          let dot_moved = {
            (* move the dot; write dot-moved item into 'dot_moved' *)
            dprod = DottedProduction.next dotted_prods item.dprod;
            lookahead = item.lookahead;
          } in

          dot_moved :: kernel_items
    ) [] [source.kernel_items.items; source.nonkernel_items]
  in

  assert (kernel_items != []);

  (* we added stuff; sorting is needed both for hashing, and also
   * for the lookahead merge step that follows a successful lookup *)
  let kernel_items = List.sort LrItem.M.compare kernel_items in
  { items = kernel_items; hash = 0 }
  


let merge_lookaheads_into dest items =
  let changed = ref false in

  (* iterate over both kernel lists simultaneously *)
  List.iter2 (fun dest_item source_item ->
    (* the caller should already have established equality of the
     * non-lookahead components of the kernel items *)
    assert (dest_item.dprod == source_item.dprod);

    let merged = TerminalSet.union dest_item.lookahead source_item.lookahead in
    if not (TerminalSet.equal dest_item.lookahead merged) then (
      dest_item.lookahead <- merged;
      changed := true
    )
  ) dest.kernel_items.items items;

  !changed


let merge_state env item_set sym in_done_list already dot_moved_items =
  let open GrammarType in
  assert (List.length dot_moved_items.items == List.length already.kernel_items.items);

  (* we already have a state with at least equal kernel items, not
   * considering their lookahead sets; so we have to merge the
   * computed lookaheads with those in 'already' *)
  if merge_lookaheads_into already dot_moved_items.items then (
    if Options._trace_lrsets () then (
      Printf.printf "from state %a, found that the transition on %a yielded a state similar to %a, but with different lookahead\n"
        Ids.State.print item_set.state_id
        Sloc.print_string (GrammarUtil.name_of_symbol env.env.index.terms env.env.index.nonterms sym)
        Ids.State.print already.state_id
    );

    (* this changed 'already'; recompute its closure *)
    item_set_closure env.env already;

    (* and reconsider all of the states reachable from it *)
    if not in_done_list then (
      (* item_sets_pending contains 'already', it will be processed later *)
    ) else (
      (* we thought we were done with this *)
      assert (ItemList.Table.mem env.item_sets_done already.kernel_items);

      (* but we're not: move it back to the 'pending' list *)
      ItemList.Table.remove env.item_sets_done already.kernel_items;
      ItemList.Stack.push already.kernel_items already env.item_sets_pending;
    )
  );

  (* use existing one for setting the transition function *)
  already


let create_state env item_set sym dot_moved_items =
  (* we don't already have it; need to actually allocate & copy *)
  let with_dot_moved = make_item_set env dot_moved_items.items in

  (* finish it by computing its closure *)
  item_set_closure env.env with_dot_moved;

  (* then add it to 'pending' *)
  ItemList.Stack.push with_dot_moved.kernel_items with_dot_moved env.item_sets_pending;

  with_dot_moved


let merge_or_create_state env item_set sym dot_moved_items =
  (* see if we already have it, in either set *)
  try
    try
      merge_state env item_set sym false (ItemList.Stack.find env.item_sets_pending dot_moved_items) dot_moved_items
    with Not_found ->
      merge_state env item_set sym true (ItemList.Table.find env.item_sets_done dot_moved_items) dot_moved_items
  with Not_found ->
    create_state env item_set sym dot_moved_items


let create_transition env item_set sym =
  (* compute the item_set produced by moving the dot across 'sym';
   * don't take closure yet since we first want to check whether it
   * is already present
   *
   * this call also yields the unused remainder of the kernel items,
   * so we can add them back in at the end *)
  let dot_moved_items =
    move_dot_no_closure
      env.env.dotted_prods
      item_set
      sym
  in

  let with_dot_moved =
    merge_or_create_state
      env
      item_set
      sym
      dot_moved_items
  in

  (* setup the transition function *)
  ItemSet.set_transition item_set sym with_dot_moved


(* for each production in the item set where the
 * dot is not at the right end *)
let process_item env item_set item =
  match LrItem.symbol_after_dot item with
  | None ->
      (* dot is at the end *)
      ()

  | Some sym ->
      if Options._trace_lrsets () then (
        print_string "%%% considering item ";
        PrintAnalysisEnv.print_lr_item env.env stdout item;
        print_newline ();
      );

      (* in LALR(1), two items might have different lookaheads; more
       * likely, re-expansions needs to propagate lookahead that
       * wasn't present from an earlier expansion
       *
       * if we already have a transition for this symbol,
       * there's nothing more to be done *)
      if Options._use_LALR1 () || ItemSet.transition item_set sym == None then
        create_transition env item_set sym


let process_item_set env =
  let item_set = ItemList.Stack.pop env.item_sets_pending in

  (* put it in the done set; note that we must do this *before*
   * the processing below, to properly handle self-loops *)
  ItemList.Table.add env.item_sets_done item_set.kernel_items item_set;

  if Options._trace_lrsets () then (
    print_string "%%% ";
    Printf.printf "state %a, %d kernel items and %d nonkernel items\n"
      Ids.State.print item_set.state_id
      (List.length item_set.kernel_items.items)
      (List.length item_set.nonkernel_items)
  );

  List.iter (process_item env item_set) item_set.kernel_items.items;
  List.iter (process_item env item_set) item_set.nonkernel_items


let construct_lr_item_sets env =
  let open GrammarType in

  let env = {
    env = env;

    nonterm_count = NtArray.length env.index.nonterms;
    term_count    = TermArray.length env.index.terms;

    next_item_set_id = 0;

    item_sets_pending = ItemList.Stack.create 13;
    item_sets_done    = ItemList.Table.create 13;
  } in

  (* start by constructing closure of first production
   * (basically assumes first production has start symbol
   * on LHS, and no other productions have the start symbol
   * on LHS) *)
  begin
    let first_dp =
      let prod_index = Ids.Production.start in
      let dp = DottedProduction.get env.env.dotted_prods prod_index 0 (* dot at left *) in

      let prod = ProdArray.get env.env.index.prods prod_index in
      let left = NtArray.get env.env.index.nonterms prod.left in

      assert (dp.prod == prod.pbase.index_id);
      assert (left.nbase.name == GrammarTreeParser.start_name);

      dp
    in

    let first_item = {
      dprod = first_dp;
      lookahead = TerminalSet.empty;
    } in

    let item_set = make_item_set env [first_item] in
    env.env.start_state <- Some item_set;

    (* EOF is not added to the lookahead; we assume EOF is actually
     * mentioned in the production already, and we won't contemplate
     * executing this reduction within the normal parser core
     * (see Glr.Engine.cleanupAfterParse) *)

    item_set_closure env.env item_set;

    (* this makes the initial pending item_set *)
    ItemList.Stack.push item_set.kernel_items item_set env.item_sets_pending
  end;

  (* for each pending item set *)
  while not (ItemList.Stack.is_empty env.item_sets_pending) do
    process_item_set env
  done;

  if true || Options._trace_lrsets () then (
    print_string "%%% ";
    Printf.printf "finished item set construction with %d states\n" (ItemList.Table.length env.item_sets_done);
  );

  let states =
    ItemList.Table.fold (fun _ item_set ids -> item_set :: ids) env.item_sets_done []
    |> List.sort (fun a b -> compare a.state_id b.state_id)
  in

  states
