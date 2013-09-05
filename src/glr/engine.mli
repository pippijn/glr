type expected =
  (ParseTables.term_index * string) list

exception ParseError of
  (*state*)ParseTables.state_id *
  (*token*)ParseTables.term_index *
  expected *
  (*cancel reason*)string option

exception Located of SourceLocation.t * exn * string

type stats = {
  num_stack_nodes	: int;
  max_stack_nodes	: int;
  det_shift		: int;
  det_reduce		: int;
  nondet_shift		: int;
  nondet_reduce		: int;
}

type 'result glr

val stats : 'a glr -> stats

val create : 'result UserActions.t -> ParseTablesType.t -> 'result glr
val parse : 'result glr -> ('lexbuf, 'token) Lexerint.lexer -> 'lexbuf -> 'result
