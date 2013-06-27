open GrammarType


let compute_supersets nonterms =
  (* first, resolve all subset nonterminals *)
  let nonterms =
    (* make a map of nonterm name -> nt_index *)
    let nonterminals = Hashtbl.create (NtArray.length nonterms) in
    NtArray.iteri (fun nt_index nonterm ->
      Hashtbl.add nonterminals (Sloc.value nonterm.nbase.name) nt_index
    ) nonterms;

    NtArray.map (fun super ->
      { super with subsets =
        List.map (fun sub ->
          (* we validated the existence of all subsets, already *)
          Hashtbl.find nonterminals (Sloc.value sub)
        ) super.subset_names
      }
    ) nonterms
  in

  let superset = NtArray.make (NtArray.length nonterms) None in

  let superset_of nt_index =
    NtArray.get superset nt_index
  in

  let set_superset_of nt_index set =
    NtArray.set superset nt_index set
  in

  NtArray.iter (fun super ->
    List.iter (fun sub ->
      match superset_of sub with
      | Some _ ->
          (* for now, only handle 'super' as a partial function *)
          failwith "nonterminal has more than one superset";
      | None ->
          set_superset_of sub (Some super.nbase.index_id)
    ) super.subsets
  ) nonterms;

  (* update nonterms *)
  NtArray.mapi (fun nt_index nonterm ->
    { nonterm with superset = NtArray.get superset nt_index }
  ) nonterms
