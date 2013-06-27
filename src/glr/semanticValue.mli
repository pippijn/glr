type t

external repr : 'a -> t = "%identity"
external obj : t -> 'a = "%identity"

val null : t
