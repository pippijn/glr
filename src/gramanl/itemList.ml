open AnalysisEnvType

(************************************************************
 * :: Structure defining operations
 ************************************************************)

module M : GrammarSig.FullType with type t = item_list = struct

  type t = item_list

  let hash_items items =
    List.fold_left (lxor) 0 (List.map LrItem.M.hash items)

  let hash a =
    if a.hash == 0 then
      a.hash <- hash_items a.items;
    a.hash

  let rec compare_items result a b =
    match a, b with
    | ah :: at, bh :: bt ->
        if result != 0 then
          result
        else
          compare_items (LrItem.M.compare ah bh) at bt
    | _ :: _, [] ->
        1
    | [], _ :: _ ->
        -1
    | [], [] ->
        result

  let compare a b =
    compare_items 0 a.items b.items

  let equal a b =
    compare a b == 0

  let stats _ = failwith "Not supported"
  let reset _ = failwith "Not supported"

  let sexp_of_t = sexp_of_item_list
  let t_of_sexp = item_list_of_sexp

  let default = {
    items = [];
    hash = 0;
  }

end

module Table = Hashtbl.Make(M)
module Map   = SexpMap.Make(M)
module Set   = SexpSet.Make(M)
module Stack = HashStack.Make(Table)
module Graph = Graph.Persistent.Digraph.ConcreteLabeled(M)(M)
