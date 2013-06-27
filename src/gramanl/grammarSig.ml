module type FullType = sig
  type t

  include Hashtbl.HashedType            with type t := t
  include Sig.OrderedConvertibleType    with type t := t
  include Graph.Sig.COMPARABLE          with type t := t
  include Graph.Sig.ORDERED_TYPE_DFT    with type t := t
end
