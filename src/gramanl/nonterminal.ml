open GrammarType

(************************************************************
 * :: Structure defining operations
 ************************************************************)

module M : GrammarSig.FullType with type t = nonterminal = struct

  type t = nonterminal

  let hash a =
    Ids.Nonterminal.hash a.nbase.index_id

  let compare a b =
    Ids.Nonterminal.compare a.nbase.index_id b.nbase.index_id

  let equal a b =
    Ids.Nonterminal.equal a.nbase.index_id b.nbase.index_id

  let stats _ = failwith "Not supported"
  let reset _ = failwith "Not supported"

  let sexp_of_t = sexp_of_nonterminal
  let t_of_sexp = nonterminal_of_sexp

  let default = empty_nonterminal

end

module Table = Hashtbl.Make(M)
module Map   = SexpMap.Make(M)
module Set   = SexpSet.Make(M)
module Stack = HashStack.Make(Table)
module Graph = Graph.Persistent.Digraph.ConcreteLabeled(M)(M)


(************************************************************
 * :: Compute graph over nonterminals
 ************************************************************)

let compute_graph nonterms prods =
  (* fold over productions *)
  ProdArray.fold_left (fun g prod ->
    let left = NtArray.get nonterms prod.left in
    (* fold over rhs *)
    List.fold_left (fun g -> function
      | Nonterminal (_, right) ->
          let right = NtArray.get nonterms right in
          Graph.add_edge g left right
      | _ ->
          g
    ) g prod.right
  ) Graph.empty prods
