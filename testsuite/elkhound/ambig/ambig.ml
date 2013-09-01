module Parser = Glr.Easy.Make
  (AmbigActions)(AmbigTables)
  (AmbigPtree)(AmbigPtreeActions)
  (AmbigTreematch)(AmbigTreematchActions)
  (AmbigTokens)
  (struct
    let ptree = false
    let typed_ptree = false
    let treematch = false
    let user = true
    let lrparse = false
  end)


let main =
  List.iter (fun file ->
    let input = open_in file in
    let lexbuf = Lexing.from_channel input in

    Parser.parse (Printf.printf "%a\n" AmbigAst.print) file AmbigLexer.token lexbuf
  )


let () =
  Printexc.record_backtrace true;
  Cmdline.run main
