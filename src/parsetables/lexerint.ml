type 'tok_type lexer = {
  token : unit -> 'tok_type;
  index : 'tok_type -> ParseTables.term_index;
  sval  : 'tok_type -> SemanticValue.t;
  sloc  : 'tok_type -> SourceLocation.t;
}
