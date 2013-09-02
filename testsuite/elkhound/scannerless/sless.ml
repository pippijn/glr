module Parser = Glr.Frontend.Make
  (SlessActions)
  (SlessTables)
  (SlessPtree)
  (SlessPtreeActions)
  (SlessTreematch)
  (SlessTreematchActions)
  (SlessTokens)
  (struct

    type lexbuf = Lexing.lexbuf
    type token = SlessTokens.t

    let from_channel file = Lexing.from_channel
    let token lexbuf =
      let token = SlessLexer.token lexbuf in
      token,
      Lexing.lexeme_start_p lexbuf,
      Lexing.lexeme_end_p lexbuf

  end)


let main inputs =
  try
    Parser.parse_files inputs
  with Glr.Frontend.ExitStatus status ->
    exit status


let () =
  Cmdline.run ~args:["-tptree"; "-print"] main
