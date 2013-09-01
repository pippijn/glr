exception ParseError of ParseTables.state_id * ParseTables.term_index
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
