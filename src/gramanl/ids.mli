module State : sig
  include Sig.IntegralModuleType

  val start : t
  val is_start : t -> bool
end


(* nonterminals *)
module Nonterminal : sig
  include Sig.IntegralModuleType

  val empty : t
  val is_empty : t -> bool

  val start : t
  val is_start : t -> bool
end


(* terminals *)
module Terminal : sig
  include Sig.IntegralModuleType

  val eof : t
  val is_eof : t -> bool
end


(* productions *)
module Production : sig
  include Sig.IntegralModuleType

  val start : t
  val is_start : t -> bool
end
