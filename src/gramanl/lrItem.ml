open AnalysisEnvType

(************************************************************
 * :: Structure defining operations
 ************************************************************)

module M : GrammarSig.FullType with type t = lr_item = struct

  type t = lr_item

  let hash a =
    DottedProduction.M.hash a.dprod

  let compare a b =
    DottedProduction.M.compare a.dprod b.dprod

  let equal a b =
    DottedProduction.M.equal a.dprod b.dprod

  let stats _ = failwith "Not supported"
  let reset _ = failwith "Not supported"

  let sexp_of_t = sexp_of_lr_item
  let t_of_sexp = lr_item_of_sexp

  let default = {
    dprod = DottedProduction.M.default;
    lookahead = TerminalSet.empty;
  }

end

module Table = Hashtbl.Make(M)
module Map   = SexpMap.Make(M)
module Set   = SexpSet.Make(M)
module Stack = HashStack.Make(Table)
module Graph = Graph.Persistent.Digraph.ConcreteLabeled(M)(M)


(************************************************************
 * :: Functions
 ************************************************************)


let is_dot_at_start item =
  DottedProduction.is_dot_at_start item.dprod


let is_dot_at_end item =
  DottedProduction.is_dot_at_end item.dprod


let symbol_after_dot item =
  DottedProduction.symbol_after_dot item.dprod


let first_includes nonterms sym t =
  let open GrammarType in
  match sym with
  | Terminal (_, term) -> term == t
  | Nonterminal (_, nonterm) ->
      (* this generalises 'isExtendingShift'.. and while this did help
       * eliminate one S/R in a grammar I was working on, there were
       * others that could not be eliminated at all (they were not
       * statically decidable), so this generalisation might not be
       * useful after all *)
      let nonterm = NtArray.get nonterms nonterm in
      TerminalSet.mem t nonterm.first


let is_extending_shift prods nonterms item nonterm term =
  let open GrammarType in
  let prod = ProdArray.get prods item.dprod.prod in
  not (is_dot_at_end item) && (* shift *)
  prod.left == nonterm && (* extending nonterm *)
  first_includes nonterms (BatOption.get (symbol_after_dot item)) term (* with t *)
