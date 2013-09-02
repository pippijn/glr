module Parser = Glr.Frontend.Make
  (AmbigActions)
  (AmbigTables)
  (AmbigPtree)
  (AmbigPtreeActions)
  (AmbigTreematch)
  (AmbigTreematchActions)
  (AmbigTokens)
  (struct

    type lexbuf = Lexing.lexbuf
    type token = AmbigTokens.t

    let from_channel = Lexing.from_channel
    let token lexbuf =
      let token = AmbigLexer.token lexbuf in
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
            Printf.printf "%a\n" AmbigAst.print result
      ))
      inputs
  with Glr.Frontend.ExitStatus status ->
    exit status


let () =
  Cmdline.run ~args:["-useract"] main
