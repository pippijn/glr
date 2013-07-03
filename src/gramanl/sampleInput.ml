open AnalysisEnvType

let (|>) = BatPervasives.(|>)


(* yield the left-context as a sequence of symbols *)
let rec left_context terms nonterms syms state =
  (* since we have the BFS tree, generating sample input (at least, if
   * it's allowed to contain nonterminals) is a simple matter of walking
   * the tree towards the root *)

  (* get the parent *)
  match state.bfs_parent with
  | None -> syms
  | Some parent ->
      (* find a symbol on which we would transition from the parent
       * to the current state *)
      let sym = ItemSet.inverse_transition terms nonterms parent state in
      left_context terms nonterms (sym :: syms) parent


let left_context terms nonterms =
  left_context (TermArray.last_index terms) (NtArray.last_index nonterms)


(* compare two-element quantities where one dominates and the other is
 * only for tie-breaking; return <0/=0/>0 if a's quantities are
 * fewer/equal/greater (TODO: this fn is a candidate for adding to a
 * library somewhere) *)
let compare_priority a_dominant b_dominant a_recessive b_recessive =
  let order = a_dominant - b_dominant in
  if order != 0 then
    order
  else
    a_recessive - b_recessive


(* for rewriting into sequences of terminals, we prefer rules with
 * fewer nonterminals on the RHS, and then (to break ties) rules with
 * fewer RHS symbols altogether; overriding all of this, if one
 * production's RHS contains a symbol already expanded, and the other
 * does not, then prefer the RHS which hasn't already been expanded *)
let compare_rewrite seen prods p1 p2 =
  let open GrammarType in

  let p1 = ProdArray.get prods p1 in
  let p2 = ProdArray.get prods p2 in

  let order =
    CoreList.foldl_until (fun prod_index ->
      let prod = ProdArray.get prods prod_index in

      let a = if GrammarUtil.rhs_has_nonterm p1 prod.left then 1 else 0 in
      let b = if GrammarUtil.rhs_has_nonterm p2 prod.left then 1 else 0 in
      a - b
    ) 0 seen
  in

  if order != 0 then
    order
  else
    compare_priority
      (GrammarUtil.num_rhs_nonterms p1)
      (GrammarUtil.num_rhs_nonterms p2)
      (List.length p1.right)
      (List.length p2.right)


(* nonterminal -> terminals *)
let rec rewrite_nt_as_terminals nonterms prods prods_by_lhs output nonterm seen =
  let open GrammarType in

  (* get all of 'nonterminal's productions that are not recursive *)
  let candidates =
    List.filter (fun prod_index ->
      let prod = ProdArray.get prods prod_index in
      (* if 'prod' has 'nonterminal' on RHS, that would certainly
       * lead to looping (though it's not the only way -- consider
       * mutual recursion), so don't even consider it *)
      not (GrammarUtil.rhs_has_nonterm prod nonterm)
      (* if this production has already been used, don't use it again *)
      && not (List.memq prod_index seen)
    ) (NtArray.get prods_by_lhs nonterm)
  in

  if candidates == [] then (
    (* I don't expect this... either the NT doesn't have any rules,
     * or all of them are recursive (which means the language doesn't
     * have any finite sentences) *)
    if Options._trace_rewrite () then (
      let nonterm = NtArray.get nonterms nonterm in
      Printf.printf "could not find any unused, non-recursive rules for %a\n"
        Sloc.print_string nonterm.nbase.name
    );

    raise Not_found
  );

  (* sort them into order of preference *)
  let candidates = List.sort (compare_rewrite seen prods) candidates in

  (* try each in turn until one succeeds; this effectively uses
   * backtracking when one fails *)
  let success =
    CoreList.foldl_until (fun prod_index ->
      let prod = ProdArray.get prods prod_index in
      try
        (* now, the chosen rule provides a RHS, which is a sequence of
         * terminals and nonterminals; recursively reduce that sequence *)
        Some (rewrite_as_terminals nonterms prods prods_by_lhs output prod.right (prod_index :: seen))
      with Not_found ->
        None
    ) None candidates
  in

  match success with
  | None -> failwith "no viable candidate found"
  | Some output -> output


(* (nonterminals and terminals) -> terminals *)
and rewrite_as_terminals nonterms prods prods_by_lhs output input seen =
  let open GrammarType in

  (* walk down the input list, creating the output list by copying
   * terminals and reducing nonterminals *)
  List.fold_left (fun output -> function
    | Terminal (_, term) -> term :: output
    | Nonterminal (_, nonterm) -> rewrite_nt_as_terminals nonterms prods prods_by_lhs output nonterm seen
  ) output input


(* given a sequence of symbols (terminals and nonterminals), use the
 * productions to rewrite it as a (hopefully minimal) sequence of
 * terminals only *)
let rewrite_as_terminals nonterms prods prods_by_lhs input =
  rewrite_as_terminals nonterms prods prods_by_lhs [] input []


(* sample input (terminals only) that can lead to a state *)
let sample_input terms nonterms prods prods_by_lhs state =
  try
    (* get left-context as terminals and nonterminals *)
    left_context terms nonterms [] state
    (* reduce the nonterminals to terminals *)
    |> rewrite_as_terminals nonterms prods prods_by_lhs
    |> List.rev_map (TermArray.get terms)
    |> List.rev_map GrammarUtil.name_of_terminal
    |> List.rev_map Sloc.value
    |> String.concat " "

  with Failure msg ->
    "Failure: " ^ msg


let left_context terms nonterms state =
  left_context terms nonterms [] state
  |> List.rev_map (GrammarUtil.name_of_symbol terms nonterms)
  |> List.rev_map Sloc.value
  |> String.concat " "
