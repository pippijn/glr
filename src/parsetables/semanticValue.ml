(* user-supplied semantic values:
 *  - Semantic values are an arbitrary word, that the user can then
 *    use as a pointer or an integer or whatever.  The parser
 *    generator inserts the appropriate casts, so the actual type
 *    I use here shouldn't ever be visible to the user.
 *  - Usually, SemanticValues that are used as pointers are considered
 *    to be owner pointers, but only in the sense that del() will be
 *    called.  It's up to the user to decide if del() actually does
 *    anything. *)
type t

external repr : 'a -> t = "%identity"
external obj : t -> 'a = "%identity"

let null = repr ()
