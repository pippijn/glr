val make_lexer
  : 'result UserActions.t
  -> ('lexbuf, 'token) Lexerint.lexer
  -> ('lexbuf, 'token) Lexerint.lexer

val make_actions
  : 'result UserActions.t
  -> ParseTablesType.t
  -> PtreeNode.t UserActions.t
