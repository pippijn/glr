exception ParseError of ParseTables.state_id * ParseTables.term_index
exception Located of SourceLocation.t * exn * string

val cancel : string -> 'a

type statistics = {
  mutable numStackNodesAllocd : int;
  mutable maxStackNodesAllocd : int;
  mutable detShift : int;
  mutable detReduce : int;
  mutable nondetShift : int;
  mutable nondetReduce : int;
}

type 'result glr

val stats_of_glr : 'a glr -> statistics

val create : 'result UserActions.t -> ParseTablesType.t -> 'result glr
val parse : 'result glr -> 'token Lexerint.lexer -> 'result
