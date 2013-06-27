open GrammarType


let compute_indexed_nonterms nonterms =
  let indexed = NtArray.make (LocStringMap.cardinal nonterms + 1) empty_nonterminal in

  (* indexed.(0) is empty_nonterminal *)
  assert (Ids.Nonterminal.is_empty empty_nonterminal.nbase.index_id);

  LocStringMap.iter (fun _ nonterm ->
    (* the ids have already been assigned *)
    let i = nonterm.nbase.index_id in (* map: symbol to index *)
    (* verify there are no duplicate indices *)
    let existing = NtArray.get indexed i in
    if existing != empty_nonterminal then (
      Printf.printf "%a has the same index (%a) as %a\n"
        Sloc.print_string existing.nbase.name
        Ids.Nonterminal.print i
        Sloc.print_string nonterm.nbase.name
    );
    assert (existing == empty_nonterminal);
    NtArray.set indexed i nonterm (* map: index to symbol *)
  ) nonterms;

  (* verify invariants *)
  NtArray.iteri (fun nt_index nonterm ->
    (* the mapping must be correct *)
    assert (nonterm.nbase.index_id == nt_index);

    (* "empty" must be the first nonterminal *)
    if Ids.Nonterminal.is_empty nonterm.nbase.index_id then
      assert (nonterm == empty_nonterminal)

    (* the synthesised start symbol must follow *)
    else if Ids.Nonterminal.is_start nonterm.nbase.index_id then
      assert (nonterm.nbase.name == GrammarTreeParser.start_name)

    (* any other nonterminals must not be empty *)
    else
      assert (nonterm != empty_nonterminal)
  ) indexed;

  (* number of nonterminals + 1 for empty_nonterminal *)
  assert (NtArray.length indexed == LocStringMap.cardinal nonterms + 1);

  NtArray.readonly indexed


let compute_indexed_terms terms =
  let indexed = TermArray.make (LocStringMap.cardinal terms) Terminal.M.default in

  LocStringMap.iter (fun _ term ->
    (* the ids have already been assigned *)
    let i = term.tbase.index_id in (* map: symbol to index *)
    TermArray.set indexed i term (* map: index to symbol *)
  ) terms;

  (* verify we filled the term_index map *)
  TermArray.iter (fun term -> assert (term != Terminal.M.default)) indexed;

  assert (TermArray.length indexed == LocStringMap.cardinal terms);

  TermArray.readonly indexed


let compute_indexed_prods productions =
  (* map: prod_index -> production *)
  let indexed = ProdArray.make (List.length productions) empty_production in

  (* fill in the map *)
  BatList.iteri (fun i prod ->
    let prod_index = Ids.Production.of_int i in
    assert (prod.pbase.index_id == prod_index);

    assert (ProdArray.get indexed prod_index == empty_production);
    ProdArray.set indexed prod_index prod;
  ) productions;

  (* verify invariants *)
  ProdArray.iteri (fun prod_index prod ->
    assert (prod.pbase.index_id == prod_index);
  ) indexed;

  (* verify we filled the prod_index map *)
  ProdArray.iter (fun prod -> assert (prod != empty_production)) indexed;

  ProdArray.readonly indexed


let compute_indices grammar =
  {
    (* build indexed terminal map *)
    terms    = compute_indexed_terms grammar.terminals;
    (* build indexed nonterminal map *)
    nonterms = compute_indexed_nonterms grammar.nonterminals;
    (* build indexed production map *)
    prods    = compute_indexed_prods grammar.productions;
  }
