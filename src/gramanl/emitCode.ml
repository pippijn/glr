module Dumper  = Camlp4.PreCast.Printers.DumpOCamlAst
module Printer = Camlp4.PreCast.Printers.OCaml

let print_interf output_file = Printer.print_interf ~output_file
let print_implem output_file = Printer.print_implem ~output_file
let  dump_interf output_file = Dumper. print_interf ~output_file
let  dump_implem output_file = Dumper. print_implem ~output_file


(************************************************
 * :: Toplevel code generators/printers
 ************************************************)


let emit_tokens name terms =
  (* Tokens *)
  let dcl = name ^ "Tokens.mli" in
  let out = name ^ "Tokens.ml" in
  let intf, impl = EmitTokens.make_ml_tokens terms in

  dump_interf dcl intf;
  dump_implem out impl


let emit_parse_tree name ptree =
  (* Parse Tree *)
  let out = name ^ "Ptree.ml" in
  let impl = EmitPtree.make_ml_parse_tree ptree in

  print_implem out impl;
  (* TODO: with sexp *)
  ignore (Sys.command
    ("sed -i -e 's/type t = \\([^;|]*\\);;/type t = \\1 with sexp;;/g;s/ | SEXP;;/ with sexp;;/g' " ^ out))


let emit_treematch name ptree =
  (* Parse Tree *)
  let out = name ^ "Treematch.tm" in
  let impl = EmitTreematch.make_ml_treematch ptree in

  BatStd.with_dispose ~dispose:close_out
    (fun out -> output_string out impl) (open_out out)


let emit_symbol_names name terms nonterms =
  (* Actions *)
  let dcl = name ^ "Names.mli" in
  let out = name ^ "Names.ml" in
  let intf, impl =
    EmitNames.make_ml_descriptions
      terms
      nonterms
  in

  dump_interf dcl intf;
  dump_implem out impl


let emit_user_actions name variant index final_prod verbatims =
  let prefix = SemanticVariant.prefix_for_variant_kind variant in

  (* Actions *)
  let dcl = name ^ prefix ^ "Actions.mli" in
  let out = name ^ prefix ^ "Actions.ml" in
  let intf, impl =
    EmitActions.make_ml_action_code
      variant index final_prod verbatims
  in

  if variant == SemanticVariant.User then (
    dump_interf dcl intf;
    dump_implem out impl;
  ) else (
    print_interf dcl intf;
    print_implem out impl;
    (* TODO: True/False *)
    ignore (Sys.command ("sed -i -e 's/\\.true/.True/;s/\\.false/.False/' " ^ out))
  )


let emit_tables name tables =
  (* Tables *)
  let dcl = name ^ "Tables.mli" in
  let out = name ^ "Tables.ml" in
  let dat = name ^ "Tables.dat" in

  let intf, impl =
    BatStd.with_dispose ~dispose:close_out
      (EmitTables.make_ml_tables tables) (open_out_bin dat)
  in

  dump_interf dcl intf;

  match impl with
  | None ->
      BatStd.with_dispose ~dispose:close_out
        (TablePrinting.print_tables tables) (open_out out)

  | Some impl ->
      dump_implem out impl


(************************************************
 * :: Main entry point
 ************************************************)

let emit_ml dirname index verbatims ptree tables =
  let open GrammarType in

  let final_prod = Ids.Production.of_int tables.ParseTablesType.finalProductionIndex in

  let name = dirname ^ "/" ^ String.lowercase (Options._module_prefix ()) in

  emit_tokens name index.terms;
  emit_parse_tree name ptree;
  emit_treematch name ptree;
  emit_symbol_names name index.terms index.nonterms;

  SemanticVariant.iter (fun variant ->
    emit_user_actions name variant index final_prod verbatims
  );

  emit_tables name tables
