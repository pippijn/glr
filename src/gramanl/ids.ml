module State = struct
  include IntegralModule

  let start : t = of_int 0
  let is_start (id : t) = id = start

  let default : t = of_int (-1)
end


(* nonterminals *)
module Nonterminal = struct
  include IntegralModule

  let empty : t = of_int 0
  let is_empty (index : t) = index = empty

  let start : t = of_int 1
  let is_start (index : t) = index = start

  let default = empty
end


(* terminals *)
module Terminal = struct
  include IntegralModule

  let eof = of_int 0
  let is_eof (index : t) = index = eof

  let default : t = of_int (-1)
end


(* productions *)
module Production = struct
  include IntegralModule

  let start : t = of_int 0
  let is_start (id : t) = id = start

  let default : t = of_int (-1)
end
