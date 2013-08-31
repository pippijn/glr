module Parser = Glr.Easy.Make
  (CcActions)(CcTables)
  (CcPtree)(CcPtreeActions)
  (CcTreematch)(CcTreematchActions)
  (CcTokens) 


let parse_ptree typed file =
  let module Parser = Parser(struct
    let ptree = not typed
    let typed_ptree = typed
    let treematch = false
    let user = false
  end) in

  let input = open_in file in
  let lexbuf = Lexing.from_channel input in

  Parser.parse (Printf.printf "result: %d\n") file CcLexer.token lexbuf


let main =
  List.iter (fun file ->
    parse_ptree false file;
    parse_ptree true file;
  )


let () =
  Printexc.record_backtrace true;
  Cmdline.run main
