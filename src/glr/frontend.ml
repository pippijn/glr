module type PtreeType = sig
  module Ptree : Sig.ConvertibleType
end

module type LexerType = sig
  type lexbuf
  type token

  val token : lexbuf -> token * Lexing.position * Lexing.position

  val from_channel : in_channel -> lexbuf
end

exception ExitStatus of int

let handle_return = function
  | Unix.WEXITED 0 ->
      ()
  | Unix.WEXITED status ->
      Printf.printf "child exited with status %d\n" status;
      raise (ExitStatus status)
  | Unix.WSIGNALED signum ->
      Printf.printf "child was killed by signal %d\n" signum;
      raise (ExitStatus 1)
  | Unix.WSTOPPED signum ->
      Printf.printf "child was stopped by signal %d\n" signum;
      raise (ExitStatus 1)


module Make
  (Actions : UserActions.S)
  (Tables : ParseTablesType.S)
  (TypedPtree : PtreeType)
  (TypedPtreeActions : UserActions.S with type result = TypedPtree.Ptree.t)
  (Treematch : PtreeType)
  (TreematchActions : UserActions.S with type result = Treematch.Ptree.t)
  (Tokens : TokenInfo.S)
  (Lexer : LexerType with type token = Tokens.t)
= struct

  open CorePervasives


  let glrparse actions tables filename lexer =
    let glr = Engine.create actions tables in

    let tree =
      try
        Some (Timing.time ~alloc:true "parsing" (Engine.parse glr) lexer ())
      with Engine.Located ((start_p, end_p), e, extra) ->
        let open Lexing in
        (* print source position *)
        Printf.printf "\n%s:%d:%d: "
          filename
          (start_p.pos_lnum)
          (start_p.pos_cnum - start_p.pos_bol + 1);

        (* print exception info *)
        begin match e with
        | Engine.ParseError (state, token) ->
            Printf.printf "parse error near \"%s\" (state: %d, token: %d)\n"
              extra state token
        | Failure msg ->
            Printf.printf "failure in user actions: %s\n\n" msg;
            print_string extra
        | e ->
            Printf.printf "exception in user actions:\n  %s\n\n"
              (Printexc.to_string e);
            print_string extra
        end;

        None
    in

    (* print accounting statistics from the parsing engine *)
    if Options._accounting () then (
      let open Engine in
      let stats = stats glr in

      Printf.printf "stack nodes: num=%d max=%d\n"
        stats.num_stack_nodes
        stats.max_stack_nodes;
      Printf.printf "LR shift:   %d\n" stats.det_shift;
      Printf.printf "LR reduce:  %d\n" stats.det_reduce;
      Printf.printf "GLR shift:  %d\n" stats.nondet_shift;
      Printf.printf "GLR reduce: %d\n" stats.nondet_reduce;
    );

    tree


  let lrparse actions tables filename lexer =
    try
      Some (Lrparse.parse actions tables lexer ())
    with
    | Failure msg ->
        Printf.printf "failure in user actions: %s\n\n" msg;
        None
    | e ->
        Printf.printf "exception in user actions:\n  %s\n\n"
          (Printexc.to_string e);
        None


  let make_lexer token = Lexerint.({
    token;
    index = (fun (t, s, e) -> Tokens.index t);
    sval  = (fun (t, s, e) -> Tokens.sval t);
    sloc  = (fun (t, s, e) -> s, e);
  })


  let rec tokenise tokens token lexbuf =
    match token lexbuf with
    | token, _, _ as next when Tokens.index token = ParseTables.eof_term ->
        if false then (
          Printf.printf "%d tokens\n" (List.length tokens);
          flush stdout;
        );
        List.rev (next :: tokens)
    | next ->
        tokenise (next :: tokens) token lexbuf


  let lexer_from_list tokens =
    let tokens = ref tokens in
    make_lexer (fun () ->
      let next = List.hd !tokens in
      tokens := List.tl !tokens;
      next
    )


  let lexer_from_dump input =
    let tokens =
      with_dispose close_in
        input_value (open_in_bin (input ^ ".tkd"))
    in
    lexer_from_list tokens, ignore


  let lexer_from_lexing lexbuf =
    make_lexer (fun () ->
      Lexer.token lexbuf
    )


  let lexer_from_file pp input =
    let lexbuf, dispose =
      let cin, dispose =
        match pp with
        | None ->
            let cin = open_in input in
            let dispose _ = close_in cin in
            (cin, dispose)

        | Some pp ->
            let cin = Unix.open_process_in (pp ^ input) in
            let dispose _ = ignore (Unix.close_process_in cin) in
            (cin, dispose)
      in
      (Lexer.from_channel cin, dispose)
    in

    assert (not (FrontendOptions._load_toks ()));
    let lexer =
      if FrontendOptions._pp () then (
        let tokens =
          Timing.time ~alloc:true "lexing" (tokenise [] Lexer.token) lexbuf
        in

        if FrontendOptions._dump_toks () then (
          if FrontendOptions._load_toks () then
            failwith "-dump-toks and -load-toks are mutually exclusive";
          with_dispose close_out
            (fun out -> output_value out tokens) (open_out_bin (input ^ ".tkd"));
        );

        lexer_from_list tokens
      ) else (
        assert (not (FrontendOptions._dump_toks ()));
        lexer_from_lexing lexbuf
      )
    in

    lexer, dispose


  let parse_file replace_lexer pp actions tables input =
    if FrontendOptions._verbose () then
      print_endline ("%%% processing " ^ input);

    let lexer, dispose =
      if FrontendOptions._load_toks () then
        lexer_from_dump input
      else
        lexer_from_file pp input
    in

    let lexer = replace_lexer lexer in

    let parse =
      if FrontendOptions._lrparse () then
        lrparse actions tables input
      else
        glrparse actions tables input
    in

    let tree =
      if FrontendOptions._tokens () then
        None
      else
        with_dispose dispose parse lexer
    in

    (input, tree)


  let dump_tree tree =
    with_dispose close_out
      (fun out -> output_value out tree) (open_out_bin "result.bin")


  let parse_files ?(replace_lexer=identity) pp actions tables files =
    let result =
      Timing.alloc "parsing all files" (
        List.rev_map (parse_file replace_lexer pp actions tables)
      ) files
    in

    if FrontendOptions._dump_tree () then
      Timing.time "serialising trees" dump_tree result;

    result


  let print_tptree tree =
    Sexplib.Sexp.output_hum stdout (TypedPtree.Ptree.sexp_of_t tree);
    print_newline ()


  let print_treematch tree =
    Sexplib.Sexp.output_hum stdout (Treematch.Ptree.sexp_of_t tree);
    print_newline ()


  let parse_files ?pp ?(action=ignore) inputs =
    let actions = Actions.userActions in
    let tables  = Tables.parseTables in

    if FrontendOptions._ptree () then (
      let actions = PtreeActions.make_actions actions tables in
      let replace_lexer = PtreeActions.make_lexer actions in
      let trees = parse_files ~replace_lexer pp actions tables inputs in

      if FrontendOptions._print () then
        List.iter (function
          | file, None -> ()
          | file, Some tree ->
              print_endline "---- result ----";
              print_string (PtreeNode.to_string tree true)
        ) trees
    );
      
    if FrontendOptions._tptree () then (
      let actions = TypedPtreeActions.userActions in
      let trees = parse_files pp actions tables inputs in

      if FrontendOptions._print () then
        List.iter (function
          | file, None -> ()
          | file, Some tree ->
              (* Print our parse tree *)
              print_endline "---- result ----";
              print_tptree tree
        ) trees
    );
    
    if FrontendOptions._treematch () then (
      let actions = TreematchActions.userActions in
      let trees = parse_files pp actions tables inputs in

      if FrontendOptions._print () then
        List.iter (function
          | file, None -> ()
          | file, Some tree ->
              (* Print the treematch parse tree *)
              print_endline "---- result ----";
              print_treematch tree
        ) trees
    );
    
    if FrontendOptions._trivial () then (
      let actions = UserActions.make_trivial actions in
      let trees = parse_files pp actions tables inputs in

      if FrontendOptions._print () then
        List.iter (function
          | file, None -> ()
          | file, Some () ->
              print_endline file
        ) trees
    );
      
    if FrontendOptions._useract () then (
      let trees = parse_files pp actions tables inputs in

      action trees
    );
  ;;

end
