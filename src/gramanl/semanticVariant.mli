type variant_kind =
  | User
  | Ptree
  | Treematch

type 'a variants

val iter : (variant_kind -> unit) -> unit
val prefix_for_variant_kind : variant_kind -> string

val variants_of_sexp : (Sexplib.Sexp.t -> 'a) -> Sexplib.Sexp.t -> 'a variants
val sexp_of_variants : ('a -> Sexplib.Sexp.t) -> 'a variants -> Sexplib.Sexp.t

val find       : variant_kind -> ('semantic -> 'a option) -> 'semantic variants -> 'a option
val add        : variant_kind -> 'semantic -> 'semantic variants -> 'semantic variants
val add_option : variant_kind -> 'semantic option -> 'semantic variants -> 'semantic variants
val set_list   : variant_kind -> 'semantic list -> 'semantic variants -> 'semantic variants

val empty : unit -> 'a variants
val singleton : variant_kind -> 'a -> 'a variants
val of_list : variant_kind -> 'a option list -> 'a variants

val combine : 'semantic variants -> 'semantic variants -> 'semantic variants
