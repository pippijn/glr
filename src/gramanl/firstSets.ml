let first_of_sequence derivable first_of seq =
  (* for each sequence member such that all
   * preceding members can derive the empty string *)
  fst (List.fold_left (fun (dest, blocked) sym ->
    let open GrammarType in
    if blocked then
      dest, true
    else
      match sym with
      (* LHS -> x alpha   means x is in First(LHS) *)
      | Terminal (_, term) ->
          let dest = TerminalSet.add term dest in

          (* stop considering RHS members since a terminal
           * effectively "hides" all further symbols from First *)
          dest, true

      | Nonterminal (_, nt_index) ->
          (* anything already in nonterm's First should be added to dest *)
          let dest = TerminalSet.union (first_of nt_index) dest in

          (* if nonterm can't derive the empty string, then it blocks
           * further consideration of right-hand side members *)
          dest, not (Derivability.can_derive_empty derivable nt_index)
  ) (TerminalSet.empty, false) seq)


let compute_prod_first first_of set_first derivable index prod =
  let open GrammarType in

  let first_of_lhs = first_of prod.left in

  (* compute First(RHS-sequence) *)
  let first_of_rhs = first_of_sequence derivable first_of prod.right in

  (* add everything in First(RHS-sequence) to First(LHS) *)
  let merged = TerminalSet.union first_of_lhs first_of_rhs in
  if TerminalSet.equal first_of_lhs merged then
    false
  else (
    set_first prod.left merged;

    if Options._trace_first () then (
      let lhs = NtArray.get index.nonterms prod.left in

      Printf.printf "added %a to %a because of %a\n"
        (PrintAnalysisEnv.print_terminal_set index.terms) first_of_rhs
        Sloc.print_string lhs.nbase.name
        (PrintGrammar.print_production index.terms index.nonterms) prod
    );

    true
  )


(* Compute, for each nonterminal, the "First" set, defined as:
 *
 *   First(N) = { x | N ->* x alpha }, where alpha is any sequence
 *                                     of terminals and nonterminals
 *
 * If N can derive the empty string, I'm going to say that empty is
 * *not* in First, despite what Aho/Sethi/Ullman says.  I do this
 * because I have that information readily as my derivable relation,
 * and because it violates the type system I've devised.
 *
 * I also don't "compute" First for terminals, since they are trivial
 * (First(x) = {x}). *)
let rec compute_first first_of set_first derivable index =
  let open GrammarType in

  (* for each production *)
  let changed =
    ProdArray.fold_left (fun changed prod ->
      compute_prod_first first_of set_first derivable index prod
      || changed
    ) false index.prods
  in

  if changed then
    (* loop until no changes *)
    compute_first first_of set_first derivable index


let compute_first derivable index =
  let open GrammarType in

  let first =
    NtArray.make (NtArray.length index.nonterms) TerminalSet.empty
  in

  compute_first (NtArray.get first) (NtArray.set first) derivable index;

  (* update nonterms *)
  let nonterms =
    NtArray.mapi (fun nt_index nonterm ->
      { nonterm with first = NtArray.get first nt_index }
    ) index.nonterms
  in

  if Options._trace_first () then (
    NtArray.iter (fun nonterm ->
      if nonterm != empty_nonterminal then (
        PrintAnalysisEnv.print_terminal_set ~name:nonterm.nbase.name index.terms stdout nonterm.first;
        print_newline ();
      )
    ) nonterms
  );

  { index with nonterms }


let compute_dprod_first derivable dotted_prods index =
  let open AnalysisEnvType in
  let open GrammarType in

  let first_of nt_index =
    let nonterm = NtArray.get index.nonterms nt_index in
    nonterm.first
  in

  (* for each production *)
  ProdArray.iteri (fun prod_index prod ->
    let dprods = ProdArray.get dotted_prods prod_index in

    (* for each dotted production where the dot is not at the end.. *)
    let rhs_length = List.length prod.right in
    for posn = 0 to rhs_length do
      let dprod = DenseIntMap.get dprods posn in
      assert (ProdArray.get index.prods dprod.prod == prod);

      let right = ExtList.nth_tl prod.right posn in

      (* compute its first *)
      let first_of_rhs = first_of_sequence derivable first_of right in
      dprod.first_set <- first_of_rhs;

      (* can it derive empty? *)
      dprod.can_derive_empty <- Derivability.can_sequence_derive_empty derivable right;

      if Options._trace_first () then (
        PrintAnalysisEnv.print_dotted_production index stdout dprod;

        if dprod.can_derive_empty then
          print_endline " - can derive empty"
        else
          print_endline " - can NOT derive empty"
      )
    done

  ) index.prods
