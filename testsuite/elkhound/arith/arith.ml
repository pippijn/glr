module Parser = Glr.Frontend.Make
  (ArithActions)
  (ArithTables)
  (ArithPtree)
  (ArithPtreeActions)
  (ArithTreematch)
  (ArithTreematchActions)
  (ArithTokens)
  (struct

    type lexbuf = Lexing.lexbuf
    type token = ArithTokens.t

    let from_channel = Lexing.from_channel
    let token lexbuf =
      let token = ArithLexer.token lexbuf in
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
            Printf.printf "%d\n" result
      ))
      inputs
  with Glr.Frontend.ExitStatus status ->
    exit status


let () =
  Cmdline.run ~args:["-useract"] main
