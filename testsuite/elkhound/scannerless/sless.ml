module Parser = Glr.Easy.Make
  (SlessActions)(SlessTables)
  (SlessPtree)(SlessPtreeActions)
  (SlessTreematch)(SlessTreematchActions)
  (SlessTokens)
  (struct
    let ptree = false
    let typed_ptree = false
    let treematch = true
    let user = false
    let lrparse = false
  end)


let main =
  List.iter (fun file ->
    let input = open_in file in
    let lexbuf = Lexing.from_channel input in

    Parser.parse (fun () -> ()) file SlessLexer.token lexbuf
  )


let () =
  Printexc.record_backtrace true;
  Cmdline.run main
