open AnalysisEnvType
open GrammarType

let (|>) = BatPervasives.(|>)


(************************************************************
 * :: General reachability of grammar elements
 ************************************************************)

let rec compute_reachable_dfs nreach treach prods prods_by_lhs nt_index =
  (* if we did not see this nonterminal, yet *)
  if not (NtSet.mem nt_index nreach) then (
    NtSet.add nt_index nreach;

    (* iterate over this nonterminal's rules *)
    List.iter (fun prod_index ->
      let prod = ProdArray.get prods prod_index in

      (* iterate over symbols in the rule RHS *)
      List.iter (function
        | Nonterminal (_, nonterm) ->
            (* recursively analyze nonterminal elements *)
            compute_reachable_dfs nreach treach prods prods_by_lhs nonterm
        | Terminal (_, term) ->
            (* just mark terminals *)
            TermSet.add term treach
      ) prod.right
    ) (NtArray.get prods_by_lhs nt_index)
  )


let compute_reachable terms prods prods_by_lhs start =
  (* start by creating the reachability bitsets *)
  let nreach = NtSet.create (NtArray.last_index prods_by_lhs) in
  let treach = TermSet.create (TermArray.last_index terms) in

  (* do a DFS on the grammar, marking things reachable as
   * they're encountered *)
  compute_reachable_dfs nreach treach prods prods_by_lhs start;

  (* the empty and start symbol are reachable *)
  NtSet.add Ids.Nonterminal.empty nreach;
  NtSet.add Ids.Nonterminal.start nreach;

  (* the EOF token is reachable *)
  TermSet.add Ids.Terminal.eof treach;

  NtSet.readonly nreach,
  TermSet.readonly treach



(************************************************************
 * :: Reachability via tagged symbols
 ************************************************************)

let rec compute_reachable_tagged_dfs reachable prods prods_by_lhs nt_index =
  (* if we did not see this nonterminal, yet *)
  if not (NtSet.mem nt_index reachable) then (
    NtSet.add nt_index reachable;

    (* iterate over this nonterminal's rules *)
    List.iter (fun prod_index ->
      let prod = ProdArray.get prods prod_index in

      (* iterate over symbols in the rule RHS *)
      List.iter (function
        | Nonterminal (Some tag, nonterm) ->
            (* recursively analyze nonterminal elements *)
            compute_reachable_tagged_dfs reachable prods prods_by_lhs nonterm

        | _ -> () (* ignore untagged and terminals *)
      ) prod.right
    ) (NtArray.get prods_by_lhs nt_index)
  )


let compute_reachable_tagged prods prods_by_lhs =
  (* start by creating the reachability bitset *)
  let reachable = NtSet.create (NtArray.last_index prods_by_lhs) in

  (* do a DFS on the grammar, marking things reachable as
   * they're encountered *)
  compute_reachable_tagged_dfs reachable prods prods_by_lhs Ids.Nonterminal.start;

  NtSet.readonly reachable
