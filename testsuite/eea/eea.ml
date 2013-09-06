module Parser = Glr.Frontend.Make
  (EeaActions)
  (EeaTables)
  (EeaPtree)
  (EeaPtreeActions)
  (EeaTreematch)
  (EeaTreematchActions)
  (EeaTokens)
  (struct

    type lexbuf = Lexing.lexbuf
    type token = EeaTokens.t

    let from_channel file = Lexing.from_channel
    let token lexbuf =
      let token = EeaLexer.token lexbuf in
      token,
      Lexing.dummy_pos,
      Lexing.dummy_pos

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
  Cmdline.run main
