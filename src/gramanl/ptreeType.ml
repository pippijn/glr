open Sexplib.Conv


type kind =
  | Native of CamlAst.ctyp
  | Alias of string Sloc.t
  | Tycon of (string Sloc.t * kind list) list
  | List of string Sloc.t
  | Option of string Sloc.t
  with sexp

type t =
  (string Sloc.t * kind) list
  with sexp
