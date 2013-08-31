module type Config = sig
  (* make it print a parse tree instead of evaluating the expression *)
  val ptree : bool
  (* produce a typed parse tree and print it as s-expressions *)
  val typed_ptree : bool
  (* produce and print treematch-backed parse tree *)
  val treematch : bool
  (* perform user actions *)
  val user : bool
end


module Make
  (Actions : UserActions.S)
  (Tables : ParseTablesType.S)
  (Ptree : PtreeActions.S)
  (PtreeAct : UserActions.S with type result = Ptree.Ptree.t)
  (Treematch : PtreeActions.S)
  (TreematchAct : UserActions.S with type result = Treematch.Ptree.t)
  (Tokens : TokenInfo.S)
  (Config : Config)
= struct

  let parse action filename actions tables lexer =
    let glr = Engine.create actions tables in

    try
      action (Engine.parse glr lexer)
    with Engine.Located ((start_p, end_p), e, extra) ->
      let open Lexing in
      (* print source position *)
      Printf.printf "\n%s:%d:%d: "
        filename
        (start_p.pos_lnum)
        (start_p.pos_cnum - start_p.pos_bol + 1);

      (* print exception info *)
      match e with
      | Engine.ParseError (state, token) ->
          Printf.printf "parse error near token \"%s\" (state: %d, token: %d)\n"
            extra
            (state :> int)
            (token :> int)
      | Failure msg ->
          Printf.printf "failure in user actions:\n  %s\n" msg;
          print_endline extra
      | e ->
          Printf.printf "exception in user actions:\n  %s\n"
            (Printexc.to_string e);
          print_endline extra


  let parse action filename token lexbuf =
    let tables = Tables.parseTables in
    let actions = Actions.userActions in

    let lexer = Lexerint.({
      token = (fun () ->
        let token = token lexbuf in
        let start_p = Lexing.lexeme_start_p lexbuf in
        let end_p = Lexing.lexeme_end_p lexbuf in
        start_p, end_p, token
      );
      index = (fun (_, _, t) -> Tokens.index t);
      sval  = (fun (_, _, t) -> Tokens.sval  t);
      sloc  = (fun (s, e, _) -> s, e);
    }) in

    if Config.ptree then (
      let actions = PtreeActions.make_actions actions tables in
      let lexer = PtreeActions.make_lexer actions lexer in

      parse (fun tree ->
        let buf = Buffer.create 128 in
        PtreeNode.print_tree tree buf true;
        print_endline "---- result ----";
        print_string (Buffer.contents buf);
      ) filename actions tables lexer
    );
    
    if Config.typed_ptree then (
      let actions = PtreeAct.userActions in

      parse (fun tree ->
        print_endline "---- result ----";
        Sexplib.Sexp.output_hum stdout
          (Ptree.Ptree.sexp_of_t tree);
        print_newline ();
      ) filename actions tables lexer
    );
    
    if Config.treematch then (
      let actions = PtreeAct.userActions in

      parse (fun tree ->
        print_endline "---- result ----";
        Sexplib.Sexp.output_hum stdout
          (Ptree.Ptree.sexp_of_t tree);
        print_newline ()
      ) filename actions tables lexer
    );
    
    if Config.user then (
      parse action filename actions tables lexer
    )

end
