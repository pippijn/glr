open CorePervasives
open Camlp4.PreCast
open GrammarType
open PtreeType

let _loc = Loc.ghost


(************************************************
 * :: Concrete syntax tree
 ************************************************)

let ctyp_of_nonterminal nonterms nonterm =
  let nonterm = NtArray.get nonterms nonterm in
  (* the type is the referenced nonterminal module *)
  nonterm.nbase.name


let ctyp_of_terminal terms term =
  let term = TermArray.get terms term in
  (* use the terminal type *)
  match Semantic.semtype_of_term SemanticVariant.User term with
  | None ->
      failwith "tagged terminals must have a declared type"
  | Some ty ->
      term.tbase.name


let ctyp_of_symbol index = function
  | Nonterminal (_, nonterm) -> ctyp_of_nonterminal index.nonterms nonterm
  | Terminal    (_,    term) -> ctyp_of_terminal index.terms term


(* XXX: if this function changes its output, PtreeMaker.prods probably
 * also needs to change *)
let production_types index term_mods left has_merge prods =
  let add_term_mod = function
    | Terminal (None, term_index) ->
        assert false

    | Terminal (Some tag, term_index) ->
        let term = TermArray.get index.terms term_index in
        begin match Semantic.semtype_of_term SemanticVariant.User term with
        | Some semtype ->
            let name = term.tbase.name in
            if not (Hashtbl.mem term_mods name) then
              Hashtbl.add term_mods name semtype
        | _ -> ()
        end
    | _ -> ()
  in

  match prods with
  | [prod] when PtreeMaker.is_singleton_nonterminal prod && not has_merge && Options._optimise () ->
      let right = PtreeMaker.right_symbol prod in
      add_term_mod right;
      let semtype = ctyp_of_symbol index right in
      (left, Alias semtype)

  | [tail; head_tail] when PtreeMaker.is_list_nonterminal tail head_tail && not has_merge && Options._optimise () ->
      let right = PtreeMaker.right_symbol head_tail in
      add_term_mod right;
      let semtype = ctyp_of_symbol index right in
      (left, List semtype)

  | [none; some] when PtreeMaker.is_option_nonterminal none some && not has_merge && Options._optimise () ->
      let right = PtreeMaker.right_symbol some in
      add_term_mod right;
      let semtype = ctyp_of_symbol index right in
      (left, Option semtype)

  | [none; some] when PtreeMaker.is_boolean_nonterminal none some && not has_merge && Options._optimise () ->
      (left, Native (Ast.(TyId (_loc, IdLid (_loc, "bool")))))

  | prods ->
      let types =
        List.map (fun prod ->
          let prod_type =
            List.map (fun sym ->
              match sym with
              | Nonterminal (None, _)
              | Terminal (None, _) ->
                  (* nothing to do for untagged symbols *)
                  []

              | _ ->
                  add_term_mod sym;
                  [Alias (ctyp_of_symbol index sym)]

            ) prod.right
            |> List.concat
          in

          let prod_name =
            match Sloc.value prod.pbase.name with
            | "" -> Sloc.generated ("P" ^ Ids.Production.to_string prod.pbase.index_id)
            | nm -> prod.pbase.name
          in

          (prod_name, Alias (Sloc.generated "SourceLocation") :: prod_type)
        ) prods
      in

      let types =
        if has_merge then
          (Sloc.generated "Merge", [Alias left; Alias left]) :: types
        else
          types
      in

      (left, Tycon types)



let make reachable index prods_by_lhs =
  let term_mods = Hashtbl.create 13 in

  let bindings =
    List.rev (NtArray.fold_left (fun bindings indices ->
      match List.map (ProdArray.get index.prods) indices with
      | [] ->
          (* the empty nonterminal has no productions *)
          bindings

      | first :: _ as prods ->
          let nonterm = NtArray.get index.nonterms first.left in
          let name = Sloc.value nonterm.nbase.name in

          if name.[0] == '_' then
            (* we do not emit code for the synthesised start rule *)
            bindings
          else (
            assert (Classify.is_uid name);

            if not (NtSet.mem first.left reachable) then
              bindings
            else
              let has_merge = Semantic.merge_of_nonterm SemanticVariant.User nonterm != None in
              let types = production_types index term_mods nonterm.nbase.name has_merge prods in
              types :: bindings
          )

    ) [] prods_by_lhs)
  in

  let term_bindings =
    Hashtbl.fold (fun name typ bindings ->
      (name, Native typ) :: bindings
    ) term_mods []
  in

  bindings @ term_bindings
