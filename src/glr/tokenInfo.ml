module type S = sig
  type t

  val name : t -> string
  val desc : t -> string
  val index : t -> ParseTables.term_index
  val sval : t -> SemanticValue.t
end
