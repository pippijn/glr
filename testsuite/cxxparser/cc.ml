module Parser = Glr.Frontend.Make
  (CcActions)
  (CcTables)
  (CcPtree)
  (CcPtreeActions)
  (CcTreematch)
  (CcTreematchActions)
  (CcTokens)
  (struct

    type lexbuf = Lexing.lexbuf
    type token = CcTokens.t

    let from_channel file = Lexing.from_channel
    let token lexbuf =
      let token = CcLexer.token lexbuf in
      token,
      Lexing.lexeme_start_p lexbuf,
      Lexing.lexeme_end_p lexbuf

  end)


let main inputs =
  try
    Parser.parse_files
      ~action:(List.iter (fun (file, result) ->
        match result with
        | None -> ()
        | Some result ->
            print_endline "---- result ----";
            print_endline result
      ))
      inputs
  with Glr.Frontend.ExitStatus status ->
    exit status


let () =
  Cmdline.run ~args:["-ptree"; "-tptree"; "-treematch"; "-useract"; "-print"] main
