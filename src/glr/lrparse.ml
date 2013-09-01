(* Deterministic LALR(1) parser. *)

let computeMaxRhsLen tables =
  CoreInt.fold_left (fun len i ->
    max len (ParseTables.getProdInfo_rhsLen tables i)
  ) 0 0 (ParseTables.getNumProds tables - 1)


let parse
  (actions : 'result UserActions.t)
  (tables  : ParseTables.t)
  (lexer   : ('lexbuf, 'token) Lexerint.lexer)
  (lexbuf  : 'lexbuf)
: 'result =

  (* make an array of semantic values for the action rule *)
  let svalArray = Array.make (computeMaxRhsLen tables) SemanticValue.null in

  let rec main_loop
    (stack   : (ParseTables.state_id * SemanticValue.t) list)
    (token   : 'token)
  : 'result =
    (* current state *)
    let state = fst (List.hd stack) in

    let tokType = lexer.Lexerint.index token in
    let tokSval = lexer.Lexerint.sval token in

    if Options._trace_parse () then
      Printf.printf "state=%d tokType=%d\n"
        state
        tokType;

    (* For small stacks, List.length is faster, for large
     * stacks, an explicit match would be faster. *)
    if tokType != ParseTables.eof_term || List.length stack > 2 then (
      (* read from action table *)
      let action = ParseTables.getActionEntry tables state tokType in

      (* shift? *)
      if ParseTables.isShiftAction tables action then (

        (* destination state *)
        let dest = ParseTables.decodeShift tables action tokType in

        if Options._trace_parse () then
          Printf.printf "shift to state %d\n" dest;

        let stack = (dest, tokSval) :: stack in

        (* next token *)
        main_loop stack Lexerint.(lexer.token lexbuf)

      (* reduce? *)
      ) else if ParseTables.isReduceAction tables action then (

        (* reduction rule *)
        let rule = ParseTables.decodeReduce tables action state in
        let rhsLen = ParseTables.getProdInfo_rhsLen tables rule in
        let lhs = ParseTables.getProdInfo_lhsIndex tables rule in

        (* move svals from the stack into the sval array *)
        let rec moveSvals svalArray i stack =
          if i = 0 then
            stack
          else
            match stack with
            | (_, sval) :: tl ->
                (*(Obj.magic svalArray).(i - 1) <- (Obj.magic sval : int);*)
                svalArray.(i - 1) <- sval;
                moveSvals svalArray (i - 1) tl
            | [] -> assert false
        in

        let stack = moveSvals svalArray rhsLen stack in

        (* invoke user's reduction action *)
        let sval =
          actions.UserActions.reductionAction rule svalArray
            Lexing.dummy_pos Lexing.dummy_pos
        in

        let next = fst (List.hd stack) in

        (* get new state *)
        let dest = ParseTables.getGoto tables next lhs in

        if Options._trace_parse () then
          Printf.printf "reduce by rule %d (len=%d, lhs=%d), goto state %d\n"
            rule rhsLen lhs dest;

        let stack = (dest, sval) :: stack in

        (* same token *)
        main_loop stack token

      (* error? *)
      ) else if ParseTables.isErrorAction tables action then (

        Printf.printf "parse error in state %d\n" state;
        failwith "parse error"

      (* bad code (ambiguity)? *)
      ) else (

        failwith "bad action code"

      )
    ) else (

      (* return value: sval of top element *)
      SemanticValue.obj (snd (List.hd stack))

    )
  in

  (* initial state *)
  let stack = [(0, SemanticValue.null)] in

  main_loop stack Lexerint.(lexer.token lexbuf)
