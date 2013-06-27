open GrammarType


let name_of_terminal { tbase = { name }; alias } =
  BatOption.default name alias


let name_of_nonterminal { nbase = { name } } =
  name


let name_of_symbol terms nonterms = function
  | Nonterminal (_, nt_index) ->
      let nonterm = NtArray.get nonterms nt_index in
      name_of_nonterminal nonterm
  | Terminal (_, term_index) ->
      let term = TermArray.get terms term_index in
      name_of_terminal term


let tag_of_symbol = function
  | Nonterminal (tag, _)
  | Terminal (tag, _) -> tag


(* symbol equality ignores tags *)
let equal_symbol a b =
  match a, b with
  | Terminal (_, term_a), Terminal (_, term_b) ->
      term_a == term_b
  | Nonterminal (_, nonterm_a), Nonterminal (_, nonterm_b) ->
      nonterm_a == nonterm_b
  | _ ->
      (* terminals never equal non-terminals *)
      false


let compare_symbol a b =
  match a, b with
  (* any state with no incoming arcs (start state) is first *)
  | None, Some _ -> -1
  | Some _, None -> 1
  | None, None -> 0

  (* terminals come before nonterminals *)
  | Some (Nonterminal _), Some (Terminal _) -> 1
  | Some (Terminal _), Some (Nonterminal _) -> -1

  (* order by id within terms/nonterms *)
  | Some (Terminal (_, term_a)), Some (Terminal (_, term_b)) ->
      Ids.Terminal.compare term_a term_b
  | Some (Nonterminal (_, nonterm_a)), Some (Nonterminal (_, nonterm_b)) ->
      Ids.Nonterminal.compare nonterm_a nonterm_b


let rhs_has_nonterm prod nonterm =
  let open GrammarType in

  try
    ignore (List.find (function
      | Terminal _ -> false
      | Nonterminal (_, nt) -> nt == nonterm
    ) prod.right);
    true
  with Not_found ->
    false


let num_rhs_nonterms prod =
  let open GrammarType in

  ExtList.count (function
    | Terminal    _ -> false
    | Nonterminal _ -> true
  ) prod.right
