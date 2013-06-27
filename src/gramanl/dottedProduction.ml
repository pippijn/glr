open AnalysisEnvType

(************************************************************
 * :: Structure defining operations
 ************************************************************)

module M : GrammarSig.FullType with type t = dotted_production = struct
  type t = dotted_production

  let hash a =
    a.dprod_id

  let compare a b =
    assert (a == b || a.dprod_id != b.dprod_id);
    a.dprod_id - b.dprod_id

  let equal a b =
    assert (a == b || a.dprod_id != b.dprod_id);
    a == b

  let stats _ = failwith "Not supported"
  let reset _ = failwith "Not supported"

  let sexp_of_t = sexp_of_dotted_production
  let t_of_sexp = dotted_production_of_sexp

  let default = {
    dprod_id = -1;
    prod = Ids.Production.default;
    dot = -1;
    after_dot = None;
    first_set = TerminalSet.empty;
    can_derive_empty = false;
    back_pointer = None;
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


let get dotted_prods prod dot =
  let open GrammarType in
  DenseIntMap.get
    (ProdArray.get dotted_prods prod) dot


let next dotted_prods dprod =
  let open GrammarType in
  DenseIntMap.get
    (ProdArray.get dotted_prods dprod.prod) (dprod.dot + 1)


let symbol_before_dot prods dprod =
  let open GrammarType in
  let prod = ProdArray.get prods dprod.prod in
  List.nth prod.right (dprod.dot - 1)


let symbol_after_dot dprod =
  dprod.after_dot


let is_dot_at_start dprod =
  dprod.dot == 0


let is_dot_at_end dprod =
  symbol_after_dot dprod == None


let back_pointer dprod =
  match dprod.back_pointer with
  | None -> failwith "no back_pointer"
  | Some item -> item


let compute_dotted_productions prods =
  let open GrammarType in

  let next_id =
    let next = ref 0 in
    fun () ->
      let id = !next in
      incr next;
      id
  in

  let dotted_prods = ProdArray.init (ProdArray.length prods) (fun i ->

    let prod = ProdArray.get prods i in
    let rhs_length = List.length prod.right in

    (* one dottedproduction for every dot position, which is one
     * more than the # of RHS elements *)
    DenseIntMap.init (rhs_length + 1) (fun dot ->
      let dot_at_end = dot == rhs_length in

      {
        prod = prod.pbase.index_id;
        dot;
        dprod_id = next_id ();
        after_dot  = (if dot_at_end then None else Some (List.nth prod.right dot));
        can_derive_empty = false;
        first_set = TerminalSet.empty;
        back_pointer = None;
      }
    )

  ) in

  (* the mapping is dense by construction, no need to verify it *)

  (* it is already readonly, too *)
  dotted_prods
