let main =
  List.iter (fun file ->
    let input = open_in file in
    let lexbuf = Lexing.from_channel input in

    Printf.printf "%d\n" (ArithParser.parse ArithLexer.token lexbuf)
  )


let () =
  Printexc.record_backtrace true;
  Cmdline.run main
