let parse_action prod =
  let open Camlp4.PreCast in

  Semantic.replace_action SemanticVariant.User prod (
    match Semantic.action_of_prod SemanticVariant.User prod with
    | None ->
        let lid id =
          let _loc, id = Sloc._loc id in
          <:expr<$lid:id$>>
        in
        (* Return tuple of all tagged rhs elements
         * or unit if nothing is tagged. *)
        begin match PtreeMaker.tags_of_production prod with
        | [] ->
            (* No rhs => return unit. *)
            let _loc = Loc.ghost in
            <:expr<()>>
        | [tag] ->
            (* One symbol => return only the symbol. *)
            lid tag
        | tag :: tags ->
            (* Multiple symbols => return a tuple. *)
            let _loc, _ = Sloc._loc tag in
            Ast.ExTup (_loc,
              List.fold_left (fun com tag ->
                Ast.ExCom (_loc, com, lid tag)
              ) (lid tag) tags
            )
        end

    | Some <:expr@loc<$str:code$>> ->
        (* Parse user action. *)
        CamlAst.expr_of_string loc code

    | Some _ ->
        failwith "trying to re-parse already parsed action code"
  )


let parse_actions = List.map parse_action
