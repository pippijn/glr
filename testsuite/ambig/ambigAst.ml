open Sexplib.Conv

type t =
  | Merge of t * t
  | Add of t * t
  | Sub of t * t
  | Mul of t * t
  | Div of t * t
  | Num of int
  with sexp


let print out ast =
  Sexplib.Sexp.output_hum out (sexp_of_t ast)
