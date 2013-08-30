open Lexing

type t = position * position

let dummy = dummy_pos, dummy_pos

let t_of_sexp sx = dummy
let sexp_of_t (s, e) = Sexplib.Sexp.Atom (
  Printf.sprintf "%d:%d-%d:%d"
    s.pos_lnum
    (s.pos_cnum - s.pos_bol + 1)
    e.pos_lnum
    (e.pos_cnum - e.pos_bol)
)
