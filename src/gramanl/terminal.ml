open GrammarType

(************************************************************
 * :: Structure defining operations
 ************************************************************)

module M : GrammarSig.FullType with type t = terminal = struct

  type t = terminal

  let hash a =
    Ids.Terminal.hash a.tbase.index_id

  let compare a b =
    Ids.Terminal.compare a.tbase.index_id b.tbase.index_id

  let equal a b =
    Ids.Terminal.equal a.tbase.index_id b.tbase.index_id

  let stats _ = failwith "Not supported"
  let reset _ = failwith "Not supported"

  let sexp_of_t = sexp_of_terminal
  let t_of_sexp = terminal_of_sexp

  let default = {
    tbase = {
      name	  = Sloc.empty_string;
      index_id	  = Ids.Terminal.default;
      semantic	  = SemanticVariant.empty ();
    };

    alias	  = None;
    precedence	  = 0;
    associativity = Sloc.dummy Assoc.AK_NONASSOC;
  }

end

module Table = Hashtbl.Make(M)
module Map   = SexpMap.Make(M)
module Set   = SexpSet.Make(M)
module Stack = HashStack.Make(Table)
module Graph = Graph.Persistent.Digraph.ConcreteLabeled(M)(M)
