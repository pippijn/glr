open Camlp4.PreCast
open GrammarType

let (|>) = BatPervasives.(|>)
let ghost = Sloc.ghost "emitTokens"


(************************************************
 * :: Tokens
 ************************************************)


let make_ml_token_type terms =
  let _loc = ghost 15 in
  TermArray.map (fun term ->
    let semtype =
      <:ctyp<$Sloc.tyUid term.tbase.name$>>
    in

    match Semantic.semtype_of_term SemanticVariant.User term with
    | None    -> semtype
    | Some ty -> <:ctyp<$semtype$ of $ty$>>
  ) terms
  |> TermArray.to_list
  |> Ast.tyOr_of_list



let make_ml_token_fn ?default value terms =
  let _loc = ghost 31 in
  let cases =
    TermArray.fold_left (fun cases term ->
      try
        let _loc, name = Sloc._loc term.tbase.name in

        let patt =
          match Semantic.semtype_of_term SemanticVariant.User term with
          | None   -> <:patt<$uid:name$>>
          | Some _ -> <:patt<$uid:name$ sval>>
        in

        let case =
          <:match_case<$patt$ -> $value term$>>
        in

        <:match_case<$cases$ | $case$>>
      with Exit ->
        cases
    ) <:match_case<>> terms
  in

  let cases =
    match default with
    | None -> cases
    | Some case -> <:match_case<$cases$ | $case$>>
  in

  <:expr<function $cases$>>


let make_ml_tokens terms =
  (* emit token type declaration in both mli and ml *)
  let types = make_ml_token_type terms in
  let intf =
    let _loc = ghost 66 in
    <:sig_item<
      type t = $types$
      include Glr.TokenInfo.S with type t := t
    >>
  in

  (* emit the token functions *)
  let name_fn =
    make_ml_token_fn (fun term ->
      let _loc, name = Sloc._loc term.tbase.name in
      <:expr<$str:name$>>
    ) terms
  in

  let desc_fn =
    make_ml_token_fn (fun { alias; tbase = { name } } ->
      match alias with
      | None ->
          let _loc, name = Sloc._loc name in
          <:expr<$str:name$>>
      | Some alias ->
          let _loc, alias = Sloc._loc alias in
          <:expr<$str:alias$>>
    ) terms
  in

  let index_fn =
    make_ml_token_fn (fun term ->
      let _loc, _ = Sloc._loc term.tbase.name in
      <:expr<$int:Ids.Terminal.to_string term.tbase.index_id$>>
    ) terms
  in

  let sval_fn =
    let _loc = ghost 101 in
    make_ml_token_fn (fun term ->
      let _loc, _ = Sloc._loc term.tbase.name in
      match Semantic.semtype_of_term SemanticVariant.User term with
      | None   -> raise Exit
      | Some _ -> <:expr<Glr.SemanticValue.repr sval>>
    ) terms ~default:<:match_case<tok -> Glr.SemanticValue.null>>
  in

  let impl =
    let _loc = ghost 110 in
    <:str_item<
      type t = $types$
      let name = $name_fn$
      let desc = $desc_fn$
      let index = $index_fn$
      let sval = $sval_fn$
    >>
  in

  intf, impl
