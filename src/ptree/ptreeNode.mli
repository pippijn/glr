type t

val make_leaf : string -> t
val make : string -> int -> (int -> t) -> t

val add_alternative : t -> t -> t

val print_tree : Format.formatter -> t -> bool -> unit
val to_string : t -> bool -> string
