module Parser = Glr.Easy.Make
  (CcActions)(CcTables)
  (CcPtree)(CcPtreeActions)
  (CcTreematch)(CcTreematchActions)
  (CcTokens) 


let parse_ptree choice file =
  let module Parser = Parser(struct
    let ptree = choice = 0
    let typed_ptree = choice = 1
    let treematch = choice = 2
    let user = choice = 3
    let lrparse = false
  end) in

  let input = open_in file in
  let lexbuf = Lexing.from_channel input in

  Parser.parse print_endline file CcLexer.token lexbuf


let main =
  List.iter (fun file ->
    parse_ptree 0 file;
    parse_ptree 1 file;
    (*parse_ptree 2 file;*)
    parse_ptree 3 file;
  )


let () =
  Printexc.record_backtrace true;
  Cmdline.run main
