open CorePervasives
open GrammarStructure
open GrammarType


let output_token out term =
  if (*term.tbase.reachable*) true then
    Printf.fprintf out "%%token %-30a\n" Sloc.print_string term.tbase.name


let output_assoc = let open Assoc in function
  | AK_LEFT -> "%left"
  | AK_RIGHT -> "%right"
  | AK_NONASSOC
  | AK_NEVERASSOC
  | AK_SPLIT -> "%nonassoc"


let output_precs out terms =
  let max_prec =
    TermArray.fold_left (fun prec term ->
      max prec term.precedence
    ) 0 terms
  in

  let precs = Array.make (max_prec + 1) [] in

  TermArray.iter (fun term ->
    if term.precedence != 0 then
      precs.(term.precedence) <- term :: precs.(term.precedence)
  ) terms;

  Array.iter (function
    | [] -> ()
    | first :: _ as terms ->
        output_string out (output_assoc (Sloc.value first.associativity));
        List.iter (fun term -> output_string out (" " ^ Sloc.value term.tbase.name)) terms;
        output_string out "\n"

  ) precs;

  ()


let output_tag out = function
  | None -> ()
  | Some tag -> output_string out (Sloc.value tag ^ "=")


let output_symbol terms nonterms out = function
  | Terminal (tag, term) ->
      let term = TermArray.get terms term in
      Printf.fprintf out " %a%a" output_tag tag Sloc.print_string term.tbase.name
  | Nonterminal (tag, nonterm) ->
      let nonterm = NtArray.get nonterms nonterm in
      Printf.fprintf out " %a%a" output_tag tag Sloc.print_string nonterm.nbase.name


let last_prec terms =
  List.fold_left (fun prec -> function
    | Terminal (_, term) ->
        let term = TermArray.get terms term in
        term.precedence
    | _ -> prec
  ) 0


let output_production out variant index prod_index =
  let prod = ProdArray.get index.prods prod_index in

  output_string out "\t|";
  if prod.right == [] then
    output_string out " /* empty */"
  else
    List.iter (output_symbol index.terms index.nonterms out) prod.right;
  if prod.prec != 0 && prod.prec != last_prec index.terms prod.right then (
    let term = TermArray.find (fun term -> term.precedence == prod.prec) index.terms in
    Printf.fprintf out " %%prec %a" Sloc.print_string term.tbase.name;
  );
  match Semantic.action_of_prod variant prod with
  | None ->
      failwith "production without semantic action"

  | Some action ->
      Printf.fprintf out "\t{ %s }\n" (CamlAst.string_of_expr action)


let output_nonterm out variant index = function
  | [] -> ()
  | first_index :: _ as indices ->
      let first = ProdArray.get index.prods first_index in
      if (*first.left.nbase.reachable*) true then (
        let left = NtArray.get index.nonterms first.left in
        Printf.fprintf out "%a:\n" Sloc.print_string left.nbase.name;
        List.iter (output_production out variant index) indices;
        output_string out "\n";
      )


let output_grammar ~file variant gram =
  let out = open_out file in

  output_string out "%{\n";
  output_string out "%}\n\n";
  TermArray.iter (output_token out) gram.gram_index.terms;
  output_string out "\n";
  output_precs out gram.gram_index.terms;
  let first = NtArray.get gram.gram_index.nonterms Ids.Nonterminal.start in
  Printf.fprintf out "\n%%start<int> %a\n\n" Sloc.print_string first.nbase.name;
  output_string out "%%\n\n";
  NtArray.iter (output_nonterm out variant gram.gram_index) gram.gram_prods_by_lhs;

  close_out out
