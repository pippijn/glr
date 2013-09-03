open CorePervasives

open Gramanl


let print_ast topforms =
  let sexpr = GrammarAst.sexp_of_topforms topforms in

  Sexplib.Sexp.output_hum stdout sexpr;
  print_newline ()


let print_grammar grammar =
  let sexpr = GrammarType.sexp_of_grammar grammar in

  Sexplib.Sexp.output_hum stdout sexpr;
  print_newline ()


let parse files =
  List.map (fun file ->
    let lexbuf = Lexing.from_channel (open_in file) in
    Lexing.(lexbuf.lex_curr_p <- {
      pos_fname = file;
      pos_lnum = 1;
      pos_bol = 0;
      pos_cnum = 0;
    });

    let state = GrammarLexer.make lexbuf in

    try
      file, GrammarParser.parse (GrammarLexer.token state) lexbuf
    with e ->
      Printf.printf "near position %d (\"%s\")\n"
        (Lexing.lexeme_start lexbuf)
        (Lexing.lexeme lexbuf);
      raise e
  ) files


let merge grammars =
  let topforms = Merge.merge grammars in
  if Options._print_merged () then
    PrintAst.print (Merge.to_ast topforms);
  topforms


let tree_parse topforms =
  let grammar = GrammarTreeParser.of_ast topforms in
  if false then
    print_grammar grammar;
  grammar


let parse_actions grammar =
  let open GrammarType in
  { grammar with
    productions = ParseActions.parse_actions grammar.productions;
  }


let make_ptree grammar =
  GrammarStructure.make grammar


let grammar_graph dirname gram =
  let open GrammarStructure in
  let open GrammarType in

  let file = dirname ^ "/grammar.dot" in
  Timing.progress "writing grammar graph to grammar.dot"
    (GrammarGraph.visualise ~file gram.gram_index.nonterms) gram.gram_index.prods;
  gram


let print_transformed dirname gram =
  Timing.progress "writing transformed grammars to grammar.gr"
    SemanticVariant.iter (fun variant ->
      if variant == SemanticVariant.User then (
        let file = dirname ^ "/grammar.gr" in
        let ast = BackTransform.ast_of_gram gram variant in
        BatPervasives.with_dispose ~dispose:close_out
          (fun out -> PrintAst.print ~out ast) (open_out file)
      )
    );
  gram


let output_menhir dirname gram =
  Timing.progress "writing menhir grammar to grammar.mly"
    SemanticVariant.iter (fun variant ->
      if variant == SemanticVariant.User then (
        let file = dirname ^ "/grammar.mly" in
        OutputMenhir.output_grammar ~file variant gram
      )
    );
  gram


let analyse gram =
  let open GrammarType in

  let env, (states, tables) = GrammarAnalysis.run_analyses gram in
  env, states, tables


let state_graph dirname (_, states, _ as tuple) =
  let file = dirname ^ "/automaton.dot" in
  Timing.progress "writing automaton graph to automaton.dot"
    (StateGraph.visualise ~file) states;
  tuple


let dump_automaton dirname (env, states, _ as tuple) =
  BatPervasives.with_dispose ~dispose:close_out
    (fun out ->
      Timing.progress "dumping states to automaton.out"
        (List.iter (PrintAnalysisEnv.print_item_set env out)) states
    ) (Pervasives.open_out (dirname ^ "/automaton.out"));
  tuple


let emit_code dirname (env, states, tables) =
  let open AnalysisEnvType in

  let index = env.index in
  let ptree = env.ptree in
  let verbatims = env.verbatims in

  try
    Timing.progress "emitting ML code"
      (EmitCode.emit_ml dirname index verbatims ptree) tables
  with Camlp4.PreCast.Loc.Exc_located (loc, e) ->
    Printf.printf "%s: Exception caught:\n  %s\n\n"
      (Camlp4.PreCast.Loc.to_string loc)
      (Printexc.to_string e);
    Printexc.print_backtrace stdout;
    exit 1


let optional enabled f x = if enabled () then f x else x

let dirname s =
  try
    let point = String.rindex s '/' in
    String.sub s 0 point
  with Not_found ->
    "."


let main inputs =
  let dirname = dirname (List.hd inputs) in

  try
    inputs
    |> Timing.progress "parsing grammar files" parse
    |> Timing.progress "merging modules" merge
    |> Timing.progress "extracting grammar structure" tree_parse
    |> Timing.progress "parsing user actions" parse_actions
    |> Timing.progress "adding parse tree actions" make_ptree
    |> optional Options._graph_grammar (grammar_graph dirname)
    |> optional Options._print_transformed (print_transformed dirname)
    |> optional Options._output_menhir (output_menhir dirname)
    |> analyse
    |> optional Options._graph_automaton (state_graph dirname)
    |> optional Options._dump_automaton (dump_automaton dirname)
    |> Valgrind.Callgrind.instrumented (emit_code dirname)
  with Diagnostics.Exit ->
    Diagnostics.print ();
    Printf.printf "Exiting on error\n";
    exit 1


let () =
  Cmdline.run main
