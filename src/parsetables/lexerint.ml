type ('lexbuf, 'token) lexer = {
  token : 'lexbuf -> 'token;
  index : 'token -> ParseTables.term_index;
  sval  : 'token -> SemanticValue.t;
  sloc  : 'token -> SourceLocation.t;
}
