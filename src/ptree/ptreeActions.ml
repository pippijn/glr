(* given actions for a grammar, wrap them with actions that
 * just build a parse tree (forest) *)

let inject  : PtreeNode.t -> SemanticValue.t = Obj.magic
let project : SemanticValue.t -> PtreeNode.t = Obj.magic



(* ------------------------ parseTreeLexer ------------------------- *)
(* wrap the lexer with one yielding parse tree leaf nodes *)
let make_lexer actions underlying =
  let open Lexerint in
  let open UserActions in
  { underlying with
    sval = (fun token ->
      inject (PtreeNode.make_leaf (actions.terminalName (underlying.index token)))
    )
  }


(* ----------------------- parseTreeActions ------------------------ *)
let make_actions underlying tables : PtreeNode.t UserActions.t =
  UserActions.({ underlying with

    (* action to perform upon performing a reduction *)
    reductionAction = (
      fun prodId svals start_p end_p ->
        (* production info *)
        let rhsLen = ParseTables.getProdInfo_rhsLen tables prodId in
        let lhsIndex = ParseTables.getProdInfo_lhsIndex tables prodId in

        let name = underlying.nonterminalName lhsIndex in

        (* make a tree node *)
        let ret = PtreeNode.make name rhsLen (fun i -> project svals.(i)) in

        inject ret
    );
      
    (* duplicate a semantic value: trivial *)
    duplicateTerminalValue = (fun _ sval -> sval);
    duplicateNontermValue  = (fun _ sval -> sval);

    (* deallocate an sval that didn't get used: trivial *)
    deallocateTerminalValue = (fun _ _ -> ());
    deallocateNontermValue  = (fun _ _ -> ());

    (* show a ptree sval *)
    showTerminalValue = (fun _ _ -> "");
    showNontermValue  = (fun _ sval -> PtreeNode.to_string (project sval) true);

    (* merge svals for alternative derivations of the same nonterminal *)
    mergeAlternativeParses = (
      fun ntIndex left right ->
        let l = project left in
        let r = project right in
        
        inject (PtreeNode.add_alternative l r)
    );

    (* choose whether to keep or drop a reduced value: trivial *)
    keepNontermValue = (fun _ _ -> true);

    (* token reclassification: trivial *)
    reclassifyToken = (fun oldTokenType _ -> oldTokenType);

  })
