open CorePervasives
open GrammarAst
open GrammarType
open Merge
open Camlp4.PreCast


let start_name = Sloc.generated "__EarlyStartSymbol"


(* synthesise a rule "__EarlyStartSymbol -> Start EOF" *)
let synthesise_start_rule topforms =
  (* find the name of the user's EOF token *)
  let TermDecl (_, eof, _) = List.find (fun (TermDecl (code, _, _)) -> code == 0) topforms.decls in

  (* build a start production *)
  let start =
    TF_nonterm ((* name = *)start_name, (* type = *)None, (* funcs = *)[], (* prods = *)[
      ProdDecl (PDK_NEW, None, [
        RH_name (Some (Sloc.generated "top"), topforms.first_nonterm);
        RH_name (None, eof);
      ], (* code: *)None)
    ], (* subsets: *)[])
  in

  { topforms with
    nonterms = LocStringMap.add start_name (start, Ids.Nonterminal.start) topforms.nonterms
  }


(* handle TF_option *)
let collect_options options config =
  List.fold_left (fun config -> function
    | TF_option (("shift_reduce_conflicts", _, _),   value) ->
        { config with expectedSR          = value }
    | TF_option (("reduce_reduce_conflicts", _, _),  value) ->
        { config with expectedRR          = value }
    | TF_option (("unreachable_nonterminals", _, _), value) ->
        { config with expectedUNRNonterms = value }
    | TF_option (("unreachable_terminals", _, _),    value) ->
        { config with expectedUNRTerms    = value }

    | _ -> failwith "merge failed"
  ) config options


let collect_verbatims verbatims =
  let verbatim, impl_verbatim =
    List.fold_left (fun (verbatim, impl_verbatim) -> function
      | TF_verbatim (true, code) ->
          let code = CamlAst.str_items_of_loc_string code in
          (verbatim, code :: impl_verbatim)
      | TF_verbatim (false, code) ->
          let code = CamlAst.sig_items_of_loc_string code in
          (code :: verbatim, impl_verbatim)

      | _ -> failwith "merge failed"
    ) ([], []) verbatims
  in

  SemanticVariant.(of_list User [
    Some (`SEM_VERBATIM verbatim);
    Some (`SEM_IMPL_VERBATIM impl_verbatim);
  ])


let collect_terminal_aliases decls =
  List.fold_left (fun aliases (TermDecl (_, name, alias)) ->
    match alias with
    | Some alias ->
        LocStringMap.add alias name aliases
    | None ->
        aliases
  ) LocStringMap.empty decls


(* type annotations *)
let collect_terminal_types types =
  let types =
    List.fold_left (fun types (TermType (name, _, _) as termtype) ->
      if LocStringMap.mem name types then
        failwith "this token already has a type";
      LocStringMap.add name termtype types
    ) LocStringMap.empty types
  in

  types


(* precedence specifications *)
let collect_terminal_precs precs aliases =
  let precs =
    List.fold_left (fun precs (PrecSpec (kind, prec, tokens) as termtype) ->
      List.fold_left (fun precs token ->
        let token =
          try
            LocStringMap.find token aliases
          with Not_found ->
            token
        in

        if prec == 0 then
          (* 0 means precedence isn't specified *)
          failwith "you can't use 0 as a precedence level, because that value is used internally to mean something else";

        if LocStringMap.mem token precs then
          failwith "this token already has a specified precedence";
        LocStringMap.add token termtype precs
      ) precs tokens
    ) LocStringMap.empty precs
  in

  precs


let spec_func funcs name formal_count =
  try
    let SpecFunc (_, params, code) =
      List.find (fun (SpecFunc (fname, _, _)) -> Sloc.value fname = name) funcs
    in

    if List.length params != formal_count then
      failwith ("incorrect number of formal parameters for '" ^ name ^ "' function");

    let code = CamlAst.expr_of_loc_string code in

    Some { params; code; }

  with Not_found ->
    None


let collect_terminals decls types precs =
  let max_index, terminals =
    List.fold_left (fun (max_index, terminals) (TermDecl (code, name, alias)) ->
      if LocStringMap.mem name terminals then
        failwith "token already declared";

      (* annotate with declared type *)
      let styp, funcs =
        try
          let (TermType (_, termtype, funcs)) = LocStringMap.find name types in
          Some termtype, funcs
        with Not_found ->
          None, []
      in

      (* apply precedence spec *)
      let associativity, precedence =
        try
          let PrecSpec (kind, prec, _) = LocStringMap.find name precs in
          kind, prec
        with Not_found ->
          Sloc.generated Assoc.AK_NONASSOC, 0
      in

      let semantic =
        SemanticVariant.(of_list User [
          BatOption.map (fun styp -> `SEM_TYPE (CamlAst.ctyp_of_loc_string styp)) styp;
          BatOption.map (fun func -> `SEM_DUP func) (spec_func funcs "dup" 1);
          BatOption.map (fun func -> `SEM_DEL func) (spec_func funcs "del" 1);
          BatOption.map (fun func -> `SEM_SHOW func) (spec_func funcs "show" 1);
          BatOption.map (fun func -> `SEM_CLASSIFY func) (spec_func funcs "classify" 1);
        ])
      in

      let index_id = Ids.Terminal.of_int code in

      let terminal = {
        tbase = {
          name;
          index_id;
          semantic;
        };
        alias;
        precedence;
        associativity;
      } in

      let max_index = max max_index index_id in

      max_index, LocStringMap.add name terminal terminals
    ) (Ids.Terminal.default, LocStringMap.empty) decls
  in

  (* track what terminals have codes *)
  let has_code = TermSet.create max_index in
  List.iter (fun (TermDecl (code, _, _)) ->
    let code = Ids.Terminal.of_int code in
    TermSet.add code has_code;
  ) decls;

  let terminals =
    (* fill in any gaps in the code space; this is required because
     * later analyses assume the terminal code space is dense *)
    Ids.Terminal.fold_left (fun terminals i ->
      if TermSet.mem i has_code then
        terminals
      else
        let dummy_name = Sloc.generated ("Dummy_filler_token" ^ Ids.Terminal.to_string i) in
        let dummy = {
          tbase = {
            name = dummy_name;
            index_id = i;
            semantic = SemanticVariant.empty ();
          };

          alias = None;
          precedence = 0;
          associativity = Sloc.generated Assoc.AK_NONASSOC;
        } in
        LocStringMap.add dummy_name dummy terminals
    ) terminals max_index
  in

  terminals


let collect_nonterminals nonterms term_count =
  (*Sexplib.Sexp.output_hum stdout (LocStringMap.sexp_of_t Gramast.sexp_of_topform nonterms);*)
  (*print_newline ();*)

  let nonterminals =
    LocStringMap.fold (fun _ (nterm, index_id) nonterminals ->
      match nterm with
      | TF_nonterm (name, styp, funcs, prods, subsets) ->
          (* record subsets *)
          List.iter (fun subset ->
            if not (LocStringMap.mem subset nonterms) then
              failwith "subsets contains non-existent nonterminal"
            (* note that, since context-free language inclusion is
             * undecidable (Hopcroft/Ullman), we can't actually check that
             * the given nonterminals really are in the subset relation *)
          ) subsets;

          let semantic =
            SemanticVariant.(of_list User [
              BatOption.map (fun styp -> `SEM_TYPE (CamlAst.ctyp_of_loc_string styp)) styp;
              BatOption.map (fun func -> `SEM_DUP func) (spec_func funcs "dup" 1);
              BatOption.map (fun func -> `SEM_DEL func) (spec_func funcs "del" 1);
              BatOption.map (fun func -> `SEM_SHOW func) (spec_func funcs "show" 1);
              BatOption.map (fun func -> `SEM_MERGE func) (spec_func funcs "merge" 2);
              BatOption.map (fun func -> `SEM_KEEP func) (spec_func funcs "keep" 1);
            ])
          in

          (* make the Grammar object to represent the new nonterminal *)
          let nonterminal = { empty_nonterminal with
            nbase = {
              name;
              index_id;
              semantic;
            };
            maximal = (BatOption.is_some (spec_func funcs "maximal" 0));
            (* we simply store the (validated) string references here, because
             * it is very hard to have cyclic immutable data structures *)
            subset_names = subsets;
          } in

          LocStringMap.add name nonterminal nonterminals

      | _ -> failwith "merge failed"
    ) nonterms LocStringMap.empty
  in

  assert (LocStringMap.cardinal nonterminals == LocStringMap.cardinal nonterms);

  nonterminals


let collect_production_rhs aliases terminals nonterminals is_synthesised rhs_list production =
  let find_nonterminal name =
    try
      LocStringMap.find name nonterminals
    with Not_found ->
      failwith ("no symbol found named " ^ Sloc.value name)
  in

  let find_terminal name =
    let terminal =
      try
        LocStringMap.find name terminals
      with Not_found ->
        let name =
          try
            LocStringMap.find name aliases
          with Not_found ->
            failwith ("terminal \"" ^ Sloc.value name ^ "\" must be defined")
        in
        LocStringMap.find name terminals
    in

    if Ids.Terminal.is_eof terminal.tbase.index_id && not is_synthesised then
      failwith "you cannot use the EOF token in your rules";
    terminal
  in

  let production =
    List.fold_left (fun production -> function
      | RH_name (tag, name) ->
          (* "empty" is a syntactic convenience; it doesn't get
           * added to the production *)
          if Sloc.equal name empty_nonterminal.nbase.name then
            production
          else
            let symbol, prec =
              try (* look up terminal *)
                let terminal = find_terminal name in
                (* whenever we see a terminal, copy its precedence spec to
                 * the production; thus, the last symbol appearing in the
                 * production will be the one that gives the precedence *)
                Terminal (tag, terminal.tbase.index_id), terminal.precedence
              with Failure _ ->
                let nonterminal = find_nonterminal name in
                (* keep old precedence *)
                Nonterminal (tag, nonterminal.nbase.index_id), production.prec
            in

            (* add it to the production *)
            { production with right = symbol :: production.right; prec }

      | RH_string (tag, str) ->
          let term = find_terminal str in
          { production with right = Terminal (tag, term.tbase.index_id) :: production.right; prec = term.precedence }

      | RH_prec (tokName) ->
          let { precedence } = find_terminal tokName in

          (* apply the specified precedence *)
          { production with prec = precedence }

      | RH_forbid (tokName) ->
          let tok = find_terminal tokName in

          let forbid =
            TerminalSet.add tok.tbase.index_id production.forbid
          in

          { production with forbid }

    ) production rhs_list
  in

  (* The list was built in reverse order; reverse it again, here *)
  { production with right = List.rev production.right }


let collect_productions aliases terminals nonterminals nonterms =
  let last_prod_index =
    LocStringMap.fold (fun _ (nterm, _) next_prod_index ->
      match nterm with
      | TF_nonterm (name, _, _, prods, _) ->
          List.fold_left (fun next_prod_index _ ->
            next_prod_index + 1
          ) next_prod_index prods
      | _ -> failwith "merge failed"
    ) nonterms 0
  in

  let productions, first_prod_index =
    LocStringMap.fold (fun _ (nterm, _) (productions, next_prod_index) ->
      match nterm with
      | TF_nonterm (name, _, _, prods, _) ->
          let left = (LocStringMap.find name nonterminals).nbase.index_id in
          (* is this the special start symbol I inserted? *)
          let is_synthesised = name == start_name in

          List.fold_left (fun (productions, next_prod_index) (ProdDecl (kind, name, rhs, action)) ->
            let semantic =
              SemanticVariant.(of_list User [
                BatOption.map (fun action -> `SEM_ACTION (CamlAst.expr_of_loc_string action)) action;
              ])
            in

            let prod_index = Ids.Production.of_int (next_prod_index - 1) in

            (* build a production *)
            let production =
              { empty_production with
                pbase = {
                  name = BatOption.default Sloc.empty_string name;
                  index_id = prod_index;
                  semantic;
                };
                left;
              }
              (* deal with RHS elements *)
              |> collect_production_rhs aliases terminals nonterminals is_synthesised rhs
            in

            (* add production to grammar *)
            production :: productions, next_prod_index - 1
          ) (productions, next_prod_index) prods

      | _ -> failwith "merge failed"
    ) nonterms ([], last_prod_index)
  in

  assert (first_prod_index == 0);
  assert (last_prod_index == List.length productions);
  productions


let of_ast topforms =
  let topforms = synthesise_start_rule topforms in

  if Options._print_merged () then
    PrintAst.print (Merge.to_ast topforms);

  let aliases = collect_terminal_aliases topforms.decls in
  let types = collect_terminal_types topforms.types in
  let precs = collect_terminal_precs topforms.precs aliases in

  (* process all (non)terminal declarations first, so while we're 
   * looking at productions we can tell if one isn't declared *)
  let terminals    = collect_terminals topforms.decls types precs in
  let verbatim     = collect_verbatims topforms.verbatims in
  let nonterminals = collect_nonterminals topforms.nonterms (LocStringMap.cardinal terminals) in

  (* process nonterminal bodies *)
  let productions  = collect_productions aliases terminals nonterminals topforms.nonterms in
  let start_symbol = topforms.first_nonterm in

  let config       = collect_options topforms.options empty_config in

  let grammar = {
    nonterminals;
    terminals;
    aliases;
    productions;
    start_symbol;

    verbatim;

    config;
  } in

  if Options._trace_merge () then (
    Printf.printf "%d terminals\n" (LocStringMap.cardinal terminals);
    Printf.printf "%d nonterminals\n" (LocStringMap.cardinal nonterminals);
    Printf.printf "%d productions\n" (List.length productions);
  );

  grammar
