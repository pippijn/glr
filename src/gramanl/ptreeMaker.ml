open CorePervasives
open GrammarType
open Camlp4.PreCast

let ghost = Sloc.ghost "ptreeMaker"


(* yield all tagged symbols *)
let symbols_of_production prod =
  List.fold_right (fun sym syms ->
    match sym with
    | Terminal (Some tag, _)
    | Nonterminal (Some tag, _) -> sym :: syms

    | _ -> syms
  ) prod.right []


let is_tail_of tail head_tail =
  let tail_syms = symbols_of_production tail in
  let head_tail_syms = symbols_of_production head_tail in

  match tail_syms, head_tail_syms with
  (* possibly empty list *)
  | [], [_; _] -> true
  (* non-empty list *)
  | [tail1], [_; tail2] -> GrammarUtil.equal_symbol tail1 tail2
  | _ -> false


let is_left_recursion head_tail =
  match symbols_of_production head_tail with
  | [Nonterminal (_, nonterm); _] -> nonterm == head_tail.left
  | _ -> false


(* nonterminal with a single production with a tagged symbol *)
let is_singleton_nonterminal prod =
  match symbols_of_production prod with
  | [sym] -> true
  | _ -> false


(* checks whether a nonterminal produces a list of another symbol *)
let is_list_nonterminal tail head_tail =
  is_tail_of tail head_tail
  && is_left_recursion head_tail


let is_option_nonterminal none some =
  symbols_of_production none == []
  && List.length (symbols_of_production some) == 1


let is_boolean_nonterminal none some =
  none.right == []
  && symbols_of_production some == []


let right_symbol head_tail =
  symbols_of_production head_tail
  |> List.rev
  |> List.hd


let nonterm_type = function
  | None ->
      let _loc = ghost 68 in
      <:ctyp<t>>
  | Some name ->
      let _loc, name = Sloc._loc name in
      <:ctyp<$uid:name$.t>>


let merge name = function
  | { params = [l; r] as params } ->
      let _loc, name = Sloc._loc name in
      `SEM_MERGE {
        params;
        code = <:expr<$uid:name$.Merge ($Sloc.exLid l$, $Sloc.exLid r$)>>
      }
  | _ ->
      failwith "invalid merge function"


let show name =
  let _loc, name' = Sloc._loc name in
  if Sloc.equal name empty_nonterminal.nbase.name
  || Sloc.equal name GrammarTreeParser.start_name then
    None
  else
    Some (
      `SEM_SHOW {
        params = [Sloc.at name "v"];
        code = <:expr<Sexplib.Sexp.to_string_hum ($uid:name'$.sexp_of_t v)>>
      }
    )


let nonterminal variant reachable nonterm =
  let is_reachable = NtSet.mem nonterm.nbase.index_id reachable in

  let semtype =
    if not is_reachable then
      let _loc = ghost 102 in
      <:ctyp<unit>>
    else if Ids.Nonterminal.is_start nonterm.nbase.index_id then
      (* synthesised start symbol *)
      nonterm_type None
    else
      nonterm_type (Some nonterm.nbase.name)
  in

  let semantic =
    SemanticVariant.of_list variant [
      Some (`SEM_TYPE semtype);

      if not is_reachable then
        None
      else
        BatOption.map (merge nonterm.nbase.name)
          (Semantic.merge_of_nonterm SemanticVariant.User nonterm);

      if not is_reachable then
        None
      else
        show nonterm.nbase.name;
    ]
  in

  let semantic =
    SemanticVariant.combine nonterm.nbase.semantic semantic
  in

  { nonterm with nbase = { nonterm.nbase with semantic } }


let nonterms variant reachable nonterms =
  NtArray.map (nonterminal variant reachable) nonterms


let check_noname nonterms prod =
  match prod.pbase.name with
  | ("", _, _) -> ()
  | (name, _, _) ->
      let left = NtArray.get nonterms prod.left in
      print_endline (
        "uselessly defined production name \"" ^ name ^ "\"" ^
        " in nonterminal \"" ^ (Sloc.value left.nbase.name) ^ "\"")


let set_action variant prod action =
  let semantic =
    SemanticVariant.set_list variant [`SEM_ACTION action] prod.pbase.semantic
  in
  { prod with pbase = { prod.pbase with semantic } }


let require_tag sym =
  match GrammarUtil.tag_of_symbol sym with
  | None -> failwith "tag required"
  | Some tag -> tag


let map_prods variant nonterms reachable has_merge prods =
  match prods with
  | [prod] when is_singleton_nonterminal prod && not has_merge && Options._optimise () ->
      let _loc = ghost 166 in
      check_noname nonterms prod;

      let tag =
        symbols_of_production prod
        |> List.hd
        |> require_tag
      in
      let action =
        Sloc.exLid tag
      in

      [set_action variant prod action]

  | [tail; head_tail] when is_list_nonterminal tail head_tail && not has_merge && Options._optimise () ->
      let _loc = ghost 181 in
      check_noname nonterms tail;
      check_noname nonterms head_tail;
      begin match symbols_of_production tail, symbols_of_production head_tail with
      (* possibly empty list *)
      | [], [head2; tail2] ->
          let head2_tag = require_tag head2 in
          let tail2_tag = require_tag tail2 in
          [
            set_action variant tail <:expr<[]>>;
            set_action variant head_tail <:expr<$Sloc.exLid tail2_tag$ :: $Sloc.exLid head2_tag$>>;
          ]
      (* non-empty list *)
      | [tail1], [head2; tail2] ->
          let tail1_tag = require_tag tail1 in
          let head2_tag = require_tag head2 in
          let tail2_tag = require_tag tail2 in
          [
            set_action variant tail <:expr<[$Sloc.exLid tail1_tag$]>>;
            set_action variant head_tail <:expr<$Sloc.exLid tail2_tag$ :: $Sloc.exLid head2_tag$>>;
          ]

      | _ -> failwith "error in is_list_nonterminal"
      end

  | [none; some] when is_option_nonterminal none some && not has_merge && Options._optimise () ->
      let _loc = ghost 207 in
      begin match symbols_of_production some with
      | [some_sym] ->
          let _loc, some_tag = Sloc._loc (require_tag some_sym) in
          [
            set_action variant none <:expr<None>>;
            set_action variant some <:expr<Some $lid:some_tag$>>;
          ]

      | _ -> failwith "error in is_option_nonterminal"
      end

  | [none; some] when is_boolean_nonterminal none some && not has_merge && Options._optimise () ->
      let _loc = ghost 220 in
      [
        set_action variant none <:expr<false>>;
        set_action variant some <:expr<true>>;
      ]

  | prods ->
      List.map (fun prod ->
        let action =
          (* production 0 is the synthesised start symbol *)
          if Ids.Production.is_start prod.pbase.index_id then (
            let _loc = ghost 231 in
            <:expr<top>>
          ) else (
            let prod_name =
              match Sloc.value prod.pbase.name with
              | "" -> Sloc.at prod.pbase.name ("P" ^ Ids.Production.to_string prod.pbase.index_id)
              | _  -> prod.pbase.name
            in

            let _loc, prod_variant =
              let left = NtArray.get nonterms prod.left in
              let _loc, prod_name = Sloc._loc prod_name in
              _loc, <:expr<$Sloc.exUid left.nbase.name$.$uid:prod_name$>>
            in

            let args =
              let pos_args =
                let _loc = ghost 248 in
                [ <:expr<(start_p, end_p)>> ]
              in

              List.fold_left (fun args sym ->
                match GrammarUtil.tag_of_symbol sym with
                | None ->
                    (* nothing to do for untagged symbols *)
                    args
                | Some tag ->
                    let _loc, tag = Sloc._loc tag in
                    <:expr<$lid:tag$>> :: args

              ) pos_args prod.right
              |> List.rev
            in

            List.fold_left (fun ctor arg ->
              <:expr<$ctor$ $arg$>>
            ) prod_variant args
          )
        in

        set_action variant prod action
      ) prods


let unit_prods variant nonterms =
  let _loc = ghost 276 in
  List.map (fun prod ->
    check_noname nonterms prod;
    set_action variant prod <:expr<()>>
  )


(* XXX: if this function changes its output, EmitPtree.production_types probably
 * also needs to change *)
let prods variant reachable nonterms prods_by_lhs prods =
  let prods_by_lhs =
    NtArray.map (fun indices ->
      let prods = List.map (ProdArray.get prods) indices in

      let is_reachable =
        match prods with
        | { left } :: _ -> NtSet.mem left reachable
        | _ -> false
      in

      let has_merge =
        match prods with
        | { left } :: _ ->
            let left = NtArray.get nonterms left in
            Semantic.merge_of_nonterm SemanticVariant.User left != None
        | _ ->
            false
      in

      if is_reachable then
        map_prods variant nonterms reachable has_merge prods
      else
        unit_prods variant nonterms prods
    ) prods_by_lhs
  in

  (* make a new indexed prods *)
  let prod_count = NtArray.sum (List.length) prods_by_lhs in
  let prods = ProdArray.make prod_count empty_production in
  NtArray.iter (List.iter (fun prod -> ProdArray.set prods prod.pbase.index_id prod)) prods_by_lhs;
  assert (not (ProdArray.memq empty_production prods));

  ProdArray.readonly prods


let verbatims variant =
  let prefix = SemanticVariant.prefix_for_variant_kind variant in
  let _loc = ghost 323 in
  SemanticVariant.of_list variant [
    Some (`SEM_VERBATIM      [ <:sig_item< open $uid:Options._module_prefix () ^ prefix$.Ptree >> ]);
    Some (`SEM_IMPL_VERBATIM [ <:str_item< open $uid:Options._module_prefix () ^ prefix$.Ptree >> ]);
  ]
