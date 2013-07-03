let rec compute_follow first_of follow_of set_follow_of derivable prods =
  let open GrammarType in
  let changed = ref false in

  (* for each production *)
  ProdArray.iter (fun prod ->
    (* for each RHS nonterminal member *)
    CoreList.iterl (fun after_right_sym right_sym ->
      match right_sym with
      | Terminal _ -> ()

      | Nonterminal (_, right_nt) ->
          (* RHS should never contain the empty string. Also, I'm not sure
           * what it means to compute Follow(empty), so let's just not do so *)
          assert (right_nt != Ids.Nonterminal.empty);

          begin
            (* rule 1:
             * if there is a production A -> alpha B beta, then
             * everything in First(beta) is in Follow(B) *)

            let follow_of_right = follow_of right_nt in

            (* compute First(beta) *)
            let first_of_beta = FirstSets.first_of_sequence derivable first_of after_right_sym in

            (* put those into Follow(right_nt) *)
            let merged = TerminalSet.union follow_of_right first_of_beta in
            if not (TerminalSet.equal follow_of_right merged) then (
              set_follow_of right_nt merged;
              changed := true;
            )
          end;

          begin
            (* rule 2:
             * if there is a production A -> alpha B, or a
             * production A -> alpha B beta where beta ->* empty ... *)
            if Derivability.can_sequence_derive_empty derivable after_right_sym then
              (* ... then everything in Follow(A) is in Follow(B) *)
              let follow_of_right = follow_of right_nt in
              let follow_of_left  = follow_of prod.left in

              let merged = TerminalSet.union follow_of_right follow_of_left in
              if not (TerminalSet.equal follow_of_right merged) then (
                set_follow_of right_nt merged;
                changed := true;
              )
          end;

    ) prod.right;

  ) prods;

  (* loop until no changes *)
  if !changed then
    compute_follow first_of follow_of set_follow_of derivable prods


let compute_follow derivable index =
  let open AnalysisEnvType in
  let open GrammarType in

  let first_of nt_index =
    let nonterm = NtArray.get index.nonterms nt_index in
    nonterm.first
  in

  let follow = NtArray.make (NtArray.length index.nonterms) TerminalSet.empty in

  let follow_of nt_index =
    NtArray.get follow nt_index
  in

  let set_follow_of nt_index set =
    NtArray.set follow nt_index set
  in

  compute_follow first_of follow_of set_follow_of derivable index.prods;

  (* update nonterms *)
  let nonterms =
    NtArray.mapi (fun nt_index nonterm ->
      { nonterm with follow = NtArray.get follow nt_index }
    ) index.nonterms
  in

  { index with nonterms }
