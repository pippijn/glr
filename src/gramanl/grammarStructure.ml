open CorePervasives
open GrammarType

type t = {
  gram_index		: GrammarType.index;
  gram_prods_by_lhs	: (Ids.Production.t list, Sig.readonly) NtArray.t;
  gram_start_nt		: Ids.Nonterminal.t;
  gram_ptree		: PtreeType.t;
  gram_options		: GrammarType.config;
  gram_verbatims	: GrammarType.global_semantic SemanticVariant.variants;
}


let compute_prods_by_lhs nonterms prods =
  (* map: nonterminal -> productions with that nonterm on LHS *)
  let prods_by_lhs = NtArray.make (NtArray.length nonterms) [] in

  ProdArray.iteri (fun prod_index prod ->
    NtArray.set prods_by_lhs prod.left
      (prod_index :: NtArray.get prods_by_lhs prod.left)
  ) prods;

  (* verify invariants *)
  NtArray.iteri (fun nt_index ->
    List.iter (fun prod_index ->
      let prod = ProdArray.get prods prod_index in
      assert (prod.left == nt_index);
    )
  ) prods_by_lhs;

  NtArray.readonly prods_by_lhs


let make grammar =
  let { terms; nonterms; prods } = GrammarIndex.compute_indices grammar in

  let prods_by_lhs = compute_prods_by_lhs nonterms prods in

  let reachable = Reachability.compute_reachable_tagged prods prods_by_lhs in

  (* construct parse tree variants *)
  let nonterms =
    nonterms
    |> PtreeMaker.nonterms SemanticVariant.Ptree     reachable
    |> PtreeMaker.nonterms SemanticVariant.Treematch reachable
  in

  let prods =
    prods
    |> PtreeMaker.prods SemanticVariant.Ptree     reachable nonterms prods_by_lhs
    |> PtreeMaker.prods SemanticVariant.Treematch reachable nonterms prods_by_lhs
  in

  let verbatims =
    grammar.verbatim
    |> SemanticVariant.combine (PtreeMaker.verbatims SemanticVariant.Ptree    )
    |> SemanticVariant.combine (PtreeMaker.verbatims SemanticVariant.Treematch)
  in

  let index = { terms; nonterms; prods } in

  {
    gram_index		= index;
    gram_start_nt	= (LocStringMap.find grammar.start_symbol grammar.nonterminals).nbase.index_id;
    gram_ptree		= PtreeStructure.make reachable index prods_by_lhs;
    gram_prods_by_lhs	= prods_by_lhs;
    gram_options	= grammar.config;
    gram_verbatims	= verbatims;
  }
