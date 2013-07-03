open AnalysisEnvType
open GrammarType


let can_derive derivable left right =
  Derivable.is_set derivable left right

let can_derive_empty derivable nonterm =
  can_derive derivable nonterm Ids.Nonterminal.empty

let can_sequence_derive_empty derivable seq =
  (* look through the sequence; if any members cannot derive
   * the empty string, fail *)
  List.for_all (function
    | Terminal _ ->
        false (* terminals can't derive the empty string *)
    | Nonterminal (_, nonterm) ->
        can_derive_empty derivable nonterm
  ) seq


let add_derivable nonterms derivable left right =
  (* Almost as an aside, I'd like to track cyclicity in grammars.
   * It's always true that N ->* N, because 0 steps are allowed.
   * A grammar is cyclic if N ->+ N, i.e. it derives itself in
   * 1 or more steps.
   *
   * We can detect that fairly easily by tracking calls to
   * this fn with left==right.  Since N ->* N in 0 steps is
   * recorded during init (and *not* by calling this fn), the
   * only calls to this with left==right will be when the
   * derivability code detects a nonzero-length path. *)
  if left == right then (
    let left = NtArray.get nonterms left in (* == right *)

    if Options._trace_derivable () then (
      if not left.cyclic then (
        print_string "%%% derivable: ";
        Printf.printf "discovered that %a ->+ %a (i.e. is cyclic)\n"
          Sloc.print_string left.nbase.name
          Sloc.print_string left.nbase.name;
      )
    );

    left.cyclic <- true; (* => right.cyclic is also true *)

    (* Even though we didn't know this already, it doesn't
     * constitute a change in the ->* relation (which is what the
     * derivability code cares about), so we do *not* report a
     * change for the cyclicity detection. *)
  );

  (* we only made a change, and hence should return true,
   * if there was a 0 here before *)
  not (Derivable.test_and_set derivable left right)


let add_derivable_nonterminal nonterms derivable left right_nonterm after_right_sym =
  (* we are wondering if prod.left can derive right_sym.. for
   * this to be true, every symbol that comes after nonterm
   * must be able to derive empty (we've already verified by
   * now that every symbol to the *left* can derive empty) *)
  let rest_derive_empty =
    (* if any symbol can't derive empty, then the sequence
     * can't derive empty *)
    List.for_all (fun sym ->
      match sym with
      | Terminal _ ->
          (* if it's a terminal, it can't derive empty *)
          if Options._trace_derivable () then (
            print_endline "terminal can't derive empty";
          );
          false
      | Nonterminal (_, nonterm) ->
          (* this symbol can't derive empty string (or, we don't
           * yet know that it can), so we conclude that prod.left
           * can't derive right_sym *)
          if Options._trace_derivable () then (
            let nonterm = NtArray.get nonterms nonterm in
            Printf.printf "nonterminal %a can derive empty?\n"
              Sloc.print_string nonterm.nbase.name;
          );
          can_derive_empty derivable nonterm
    ) after_right_sym
  in

  if Options._trace_derivable () then (
    let left = NtArray.get nonterms left in
    Printf.printf "%a's rest can %sderive empty\n"
      Sloc.print_string left.nbase.name
      (if rest_derive_empty then "" else "NOT ");
  );

  if rest_derive_empty then (
    (* we have discovered that prod.left can derive right_sym *)
    let added = add_derivable nonterms derivable left right_nonterm in
    assert added; (* above, we verified we didn't already know this *)

    if Options._trace_derivable () then (
      let left = NtArray.get nonterms left in
      let right_nonterm = NtArray.get nonterms right_nonterm in
      print_string "%%% derivable: ";
      Printf.printf "discovered (by production): %a ->* %a\n"
        Sloc.print_string left.nbase.name
        Sloc.print_string right_nonterm.nbase.name;
    );

    true
  ) else (
    false
  )


let add_derivable_relations index derivable changes =
  ProdArray.iter (fun prod ->
    if Options._trace_derivable () then (
      PrintGrammar.print_production index.terms index.nonterms stdout prod;
      print_newline ();
    );

    match prod.right with
    | [] ->
        (* since I don't include 'empty' explicitly in my rules, I won't
         * conclude that anything can derive empty, which is a problem;
         * so I special-case it here *)
        ignore (add_derivable index.nonterms derivable prod.left Ids.Nonterminal.empty)

    | right ->
        (* iterate over RHS symbols, seeing if the LHS can derive that
         * RHS symbol (by itself) *)
        ignore (CoreList.fold_leftl (fun derives after_right_sym right_sym ->
          derives &&
            match right_sym with
            | Terminal _ ->
                (* if prod.left derives a string containing a terminal,
                 * then it can't derive any nontermial alone (using this
                 * production, at least) -- empty is considered a nonterminal *)
                false

            | Nonterminal (_, right_nonterm) ->
                (* check if we already know that LHS derives this nonterm *)
                if can_derive derivable prod.left right_nonterm then
                  (* we already know that prod.left derives right_sym,
                   * so let's not check it again *)
                  ()
                else if add_derivable_nonterminal index.nonterms derivable prod.left right_nonterm after_right_sym then
                  incr changes;

                (* ok, we've considered prod.left deriving right_sym.  now, we
                 * want to consider whether prod.left can derive any of the
                 * symbols that follow right_sym in this production.  for this
                 * to be true, right_sym itself must derive the empty string
                 *
                 * if it doesn't -- no point in further consideration of
                 * this production *)
                can_derive_empty derivable right_nonterm
        ) true right)

  ) index.prods


let compute_derivability_closure nonterms derivable changes =
  (* I'll do this by computing R + R^2 -- that is, I'll find all
   * paths of length 2 and add an edge between their endpoints.
   * I do this, rather than computing the entire closure now, since
   * on the next iter I will add more relations and have to re-do
   * a full closure; iterative progress seems a better way.
   *
   * I don't consider edges (u,u) because it messes up my cyclicity
   * detection logic.  (But (u,v) and (v,u) is ok, and in fact is
   * what I want, for detecting cycles.) *)
  let nonterm_count = NtArray.length nonterms in
  (* for each node u (except empty) *)
  for u = 1 to nonterm_count - 1 do
    let u = Ids.Nonterminal.of_int u in
    (* for each edge (u,v) where u != v *)
    for v = 0 to nonterm_count - 1 do
      let v = Ids.Nonterminal.of_int v in
      if u != v && can_derive derivable u v then
        (* for each edge (v,w) where v != w *)
        for w = 0 to nonterm_count - 1 do
          let w = Ids.Nonterminal.of_int w in
          if v != w && can_derive derivable v w then
            (* add an edge (u,w), if there isn't one already *)
            if add_derivable nonterms derivable u w then (
              if Options._trace_derivable () then (
                print_string "%%% derivable: ";
                Printf.printf "discovered (by closure step): %a ->* %a\n"
                  Sloc.print_string (NtArray.get nonterms u).nbase.name
                  Sloc.print_string (NtArray.get nonterms w).nbase.name;
              );
              incr changes
            )
        done
    done
  done


let initial_derivable_relation last_nonterm =
  (* two-dimensional bit matrix to represent token derivabilities *)
  let derivable = Derivable.create last_nonterm last_nonterm in

  Ids.Nonterminal.iter (fun i ->
    (* every nonterminal can derive itself in 0 or more steps
     * (specifically, in 0 steps, at least) *)
    Derivable.set derivable i i
  ) last_nonterm;

  derivable


let compute_derivability_relation index =
  (* start off with 1 so the loop is entered *)
  let changes = ref 1 in

  let derivable = initial_derivable_relation (NtArray.last_index index.nonterms) in

  (* iterate: propagate 'true' bits across the derivability matrix
   * (i.e. compute transitive closure on the can_derive relation) *)
  while !changes != 0 do
    changes := 0;

    (* first part: add new can_derive relations *)
    add_derivable_relations index derivable changes;
    let new_relations = !changes in
    if Options._trace_derivable () then
      Printf.printf "%d new relations\n" new_relations;

    (* second part: compute closure over existing relations *)
    compute_derivability_closure index.nonterms derivable changes;
    if Options._trace_derivable () then
      Printf.printf "%d relations by closure\n" (!changes - new_relations);

  done;

  if Options._trace_derivable () then
    Derivable.print derivable;

  Derivable.readonly derivable
