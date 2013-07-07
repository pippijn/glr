open CorePervasives
open GrammarType
open AnalysisEnvType


let compute_reachable env =
  let reachable_nonterms, reachable_terms =
    Timing.progress "reachability computation"
      (Reachability.compute_reachable env.index.terms env.index.prods env.prods_by_lhs) env.start_nt
  in

  let unr_nonterms =   NtArray.length env.index.nonterms -   NtSet.cardinal reachable_nonterms in
  let unr_terms    = TermArray.length env.index.terms    - TermSet.cardinal reachable_terms in

  if false then (
    NtArray.iter (fun nonterm ->
      if not (NtSet.mem nonterm.nbase.index_id reachable_nonterms) then
        Printf.printf "unreachable nonterminal: %a\n" Sloc.print_string nonterm.nbase.name
    ) env.index.nonterms;
    TermArray.iter (fun term ->
      if not (TermSet.mem term.tbase.index_id reachable_terms) then
        Printf.printf "unreachable terminal: %a\n" Sloc.print_string term.tbase.name
    ) env.index.terms;
  );

  Warnings.report_unexpected unr_nonterms env.options.expectedUNRNonterms "unreachable nonterminals";
  Warnings.report_unexpected unr_terms env.options.expectedUNRTerms "unreachable terminals";
;;



let compute_grammar_properties env =
  compute_reachable env;

  let index = env.index in

  let derivable =
    Timing.progress "derivability computation"
      Derivability.compute_derivability_relation index
  in

  let index = { index with
    nonterms =
      Timing.progress "super sets computation"
        SuperSets.compute_supersets index.nonterms;
  } in

  let index =
    Timing.progress "first sets computation"
      (FirstSets.compute_first derivable) index;
  in

  Timing.progress "computation of dotted production first sets"
    (FirstSets.compute_dprod_first derivable env.dotted_prods) index;

  let index =
    Timing.progress "follow sets computation"
      (FollowSets.compute_follow derivable) index
  in

  { env with derivable; index }


let compute_lr_tables env =
  let states =
    Timing.progress "LR item set construction" LrItemSets.construct_lr_item_sets env
    |> Timing.progress "renumbering states" (Renumbering.renumber_states env)
    |> Timing.progress "BFS tree on state graph" (BfsTree.compute_bfs_tree env)
  in
  let tables =
    states
    |> Timing.progress "parse table construction"
         (TableConstruction.compute_parse_tables env (*~allow_ambig:*)true)
  in

  states, tables


let init_env gram =
  let open GrammarStructure in
  (* build dotted productions for each production *)
  let dotted_prods = DottedProduction.compute_dotted_productions gram.gram_index.prods in

  (* make the env *)
  let env = {
    index = gram.gram_index;
    start_nt = gram.gram_start_nt;
    ptree = gram.gram_ptree;
    prods_by_lhs = gram.gram_prods_by_lhs;
    dotted_prods;
    derivable = Derivable.empty;
    start_state = None;

    options = gram.gram_options;
    verbatims = gram.gram_verbatims;
  } in

  env


let run_analyses gram =
  let env = compute_grammar_properties (init_env gram) in

  env, compute_lr_tables env
