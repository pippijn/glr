open AnalysisEnvType


type decision = {
  keep_shift : bool;
  keep_reduce : bool;
  warn : bool;
}


let print_actions terms nonterms shift_dest reductions =
  begin match shift_dest with
  | Some shift_dest ->
      Printf.printf "      | shift, and move to state %a\n" Ids.State.print shift_dest.state_id
  | None -> ()
  end;

  List.iter (fun prod ->
    Printf.printf "      | reduce by rule %a\n"
      (PrintGrammar.print_production terms nonterms) prod;
  ) reductions


(* decide what to do, and return the result in the first
 * two tuple members, keep_shift and keep_reduce *)
let handle_shift_reduce_conflict index state prod sym decision =
  let open GrammarType in

  if Options._trace_prec () then (
    Printf.printf "    in state %a, S/R conflict on token %a with production %a\n"
      Ids.State.print state.state_id
      Sloc.print_string sym.tbase.name
      (PrintGrammar.print_production index.terms index.nonterms) prod;
  );

  (* look at scannerless directives *)
  let shift_extends_super =
    (* is this nonterm or any of its declared supersets maximal? *)
    let rec loop super maximal =
      match super.superset with
      | Some superset when not maximal ->
          let superset = NtArray.get index.nonterms superset in
          loop superset superset.maximal
      | _ ->
          super, maximal
    in

    let left = NtArray.get index.nonterms prod.left in
    let super, maximal = loop left left.maximal in

    (* see if this reduction can be removed due to a 'maximal' spec;
     * in particular, is the shift going to extend 'super'? *)
    maximal && ItemSet.has_extending_shift index.prods index.nonterms state super.nbase.index_id sym.tbase.index_id
  in

  (* scannerless *)
  if shift_extends_super then (
    if Options._trace_prec () then (
      print_endline "      => resolved in favor of SHIFT due to maximal munch";
    );
    (* remove reduction due to maximal munch disambiguation *)
    { decision with keep_reduce = false }

  (* precedence *)
  ) else if prod.prec == 0 || sym.precedence == 0 then (
    if Options._trace_prec () then (
      print_endline "      => will SPLIT because no disambiguation spec available";
    );
    (* one of the two doesn't have a precedence specification,
     * so we can do nothing *)
    decision
  ) else if prod.prec > sym.precedence then (
    if Options._trace_prec () then (
      print_endline "      => resolved in favor of REDUCE due to precedence";
    );
    (* production's precedence is higher, so we choose to reduce
     * instead of shift *)
    { decision with keep_shift = false }
  ) else if prod.prec < sym.precedence then (
    if Options._trace_prec () then (
      print_endline "      => resolved in favor of SHIFT due to precedence";
    );
    (* symbol's precedence is higher, so we shift *)
    { decision with keep_reduce = false }

  (* precedences are equal, so we look at associativity (of token) *)
  ) else (
    let open Assoc in
    match Sloc.value sym.associativity with
    | AK_LEFT ->
        if Options._trace_prec () then (
          print_endline "      => resolved in favor of REDUCE due to associativity";
        );
        { decision with keep_shift = false }
    | AK_RIGHT ->
        if Options._trace_prec () then (
          print_endline "      => resolved in favor of SHIFT due to associativity";
        );
        { decision with keep_reduce = false }
    | AK_NONASSOC ->
        if Options._trace_prec () then (
          print_endline "      => removed BOTH alternatives due to nonassociativity";
        );
        { decision with keep_shift = false; keep_reduce = false }
    | AK_NEVERASSOC ->
        (* the user claimed this token would never be involved in a conflict *)
        failwith (Printf.sprintf "token %s was declared 'prec', but it is involved in an associativity conflict with \"%s\" in state %a\n"
          (Sloc.value sym.tbase.name)
          (* TODO *)"prod"
          Ids.State.sprint state.state_id)
    | AK_SPLIT ->
        if Options._trace_prec () then (
          print_endline "      => will SPLIT because user asked to";
        );
        (* the user does not want disambiguation of this *)
        { decision with warn = false }
  )


(* static disambiguation for S/R conflicts *)
let disambiguate_shift_reduce_conflict index state sym shift_dest reductions suppressed_warnings =
  match shift_dest with
  | Some _ ->
      (* we have (at least) a shift/reduce conflict, which is the
       * situation in which prec/assoc specifications are used; consider
       * all the possible reductions, so we can resolve S/R conflicts
       * even when there are R/R conflicts present too *)
      List.fold_left (fun (shift_dest, reductions) prod ->
        let { keep_shift; keep_reduce; warn; } =
          handle_shift_reduce_conflict index state prod sym { keep_shift = true; keep_reduce = true; warn = true; }
        in

        if not warn then
          incr suppressed_warnings;

        match keep_shift, keep_reduce with
        | false, false -> None      ,         reductions (* drop both *)
        | false, true  -> None      , prod :: reductions (* drop shift *)
        | true , false -> shift_dest,         reductions (* drop reduction *)
        | true , true  -> shift_dest, prod :: reductions (* keep both *)
      ) (shift_dest, []) reductions

      (* there is still a potential for misbehavior.. e.g., if there are two
       * possible reductions (R1 and R2), and one shift (S), then the user
       * could have specified prec/assoc to disambiguate, e.g.
       *   R1 < S
       *   S < R2
       * so that R2 is the right choice; but if I consider (S,R2) first,
       * I'll simply drop S, leaving no way to disambiguate R1 and R2 ..
       * for now I'll just note the possibility... *)

  | None ->
      shift_dest, reductions


(* the idea is we might be trying to do scannerless parsing, and
 * someone might say that Identifier has as subsets all the keywords,
 * so competing reductions should favor the subsets (the keywords) *)
let subset_directive_resolution nonterms state sym reductions =
  let open GrammarType in

  (* make a map of which nonterminals appear on the LHS of one
   * of the reductions, and has a superset *)
  let map =
    List.fold_left (fun map prod ->
      let left = NtArray.get nonterms prod.left in
      if BatOption.is_some left.superset then
        NonterminalSet.add left.nbase.index_id map
      else
        map
    ) NonterminalSet.empty reductions
  in

  if NonterminalSet.is_empty map then (
    reductions (* nothing we can do *)
  ) else (
    (* walk over the reductions, removing those that have reductions
     * to subsets also in the list *)
    List.fold_left (fun reductions prod ->
      let left = NtArray.get nonterms prod.left in

      let remove =
        List.exists (fun sub ->
          let remove = NonterminalSet.mem sub map in
          if remove then (
            if Options._trace_prec () then (
              let sub = NtArray.get nonterms sub in
              Printf.printf "in state %a, R/R conflict on token %a, removed production yielding %a, because another yields subset %a\n"
                Ids.State.print state.state_id
                Sloc.print_string sym.tbase.name
                Sloc.print_string left.nbase.name
                Sloc.print_string sub.nbase.name;
            );
          );

          remove
        ) left.subsets
      in
      
      if remove then (
        reductions
      ) else (
        prod :: reductions
      )
    ) [] reductions
  )

(* how many actions are there? *)
let actions shift_dest reductions =
  (if BatOption.is_some shift_dest then 1 else 0) + List.length reductions


let try_resolve_conflicts index state sym shift_dest reductions allow_ambig sr rr =
  let open GrammarType in

  (* count how many warning suppressions we have *)
  let suppressed_warnings = ref 0 in

  let shift_dest, reductions =
    disambiguate_shift_reduce_conflict index state sym shift_dest reductions suppressed_warnings
  in

  (* static disambiguation for R/R conflicts *)
  let reductions =
    if List.length reductions == 1 then (
      reductions
    ) else (
      let open GrammarType in
      (* find the highest precedence *)
      let highest_prec =
        List.fold_left (fun prec prod ->
          max prec prod.prec
        ) 0 reductions
      in

      (* remove any productions that are lower than 'highest_prec' *)
      List.fold_left (fun reductions prod ->
        if prod.prec != 0 && prod.prec < highest_prec then (
          if Options._trace_prec () then (
            Printf.printf "in state %a, R/R conflict on token %a, removed production %a because %d < %d\n"
              Ids.State.print state.state_id
              Sloc.print_string sym.tbase.name
              (PrintGrammar.print_production index.terms index.nonterms) prod
              prod.prec
              highest_prec;
          );
          reductions
        ) else (
          prod :: reductions
        )
      ) [] reductions
    )
  in

  (* additional R/R resolution using subset directives *)
  let reductions =
    if List.length reductions == 1 then (
      reductions
    ) else (
      subset_directive_resolution index.nonterms state sym reductions
    )
  in

  (* after the disambiguation, maybe now there's no conflicts?
   * or, if conflicts remain, did we get at least that many warning
   * suppressions? *)
  let actions = actions shift_dest reductions in
  if actions - !suppressed_warnings <= 1 then (
    (* don't print information about conflicts *)
  ) else (
    begin match shift_dest with
    | Some shift_dest ->
        incr sr; (* shift/reduce conflict *)
        rr := !rr + actions - 2 (* any reduces beyond first are r/r errors *)

    | None ->
        rr := !rr + actions - 1 (* all reduces beyond first are r/r errors *)
    end;

    if Options._trace_conflict () then (
      let open GrammarType in
      Printf.printf "    conflict for symbol %a\n" Sloc.print_string sym.tbase.name;
      print_actions index.terms index.nonterms shift_dest reductions;
    );
  );

  if not allow_ambig && actions > 1 then (
    (* force only one action, using Bison's disambiguation:
     *   - prefer shift to reduce
     *   - prefer the reduction which occurs first in the grammar file *)
    match shift_dest with
    | Some _ -> shift_dest, []
    | None ->
        None, [
          List.fold_left (fun reduction prod ->
            let open GrammarType in
            (* production indices happen to be assigned in file order *)
            if prod.pbase.index_id > reduction.pbase.index_id then
              prod
            else
              reduction
          ) (List.hd reductions) (List.tl reductions)
        ]
  ) else (
    shift_dest, reductions
  )


(* given some potential parse actions, apply available disambiguation
 * to remove some of them; print warnings about conflicts, in some
 * situations *)
let resolve_conflicts index (* indexed grammar structure arrays *)
		      state (* parse state in which the actions are possible *)
                      sym (* lookahead symbol for these actions *)
                      shift_dest (* (option) the state to which we can shift *)
                      reductions (* list of possible reductions *)
                      allow_ambig (* if false, always return at most 1 action *)
                      sr rr (* counts of S/R and R/R conflicts, resp. *) =
  let actions = actions shift_dest reductions in
  if Options._trace_conflict () then (
    if actions >= 1 then (
      let open GrammarType in
      print_string "%%% before conflict resolution";
      Printf.printf " (on token %a):\n" Sloc.print_string sym.tbase.name;
      print_actions index.terms index.nonterms shift_dest reductions;
    );
  );

  if actions <= 1 then (
    (* no conflict *)
    shift_dest, reductions
  ) else (
    try_resolve_conflicts index state sym shift_dest reductions allow_ambig sr rr
  )
