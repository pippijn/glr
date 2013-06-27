open GrammarType

let print_tag out = function
  | None -> ()
  | Some tag ->
      let tag = Sloc.value tag in
      Printf.fprintf out "%s:" tag


let print_sloc out (t, s, e) =
  output_string out t


let print_symbol terms nonterms out sym =
  match sym with
  | Terminal (tag, term) ->
      print_tag out tag;
      print_sloc out (GrammarUtil.name_of_symbol terms nonterms sym)
  | Nonterminal (tag, nonterm) ->
      print_tag out tag;
      print_sloc out (GrammarUtil.name_of_symbol terms nonterms sym)


let print_production terms nonterms out prod =
  let left = NtArray.get nonterms prod.left in

  Printf.fprintf out "  [%a (%a)] %a ->"
    Ids.Production.print prod.pbase.index_id
    Ids.Production.print prod.pbase.index_id
    print_sloc left.nbase.name;

  if prod.right == [] then
    output_string out " empty"
  else
    List.iter (fun sym ->
      output_string out " ";
      print_symbol terms nonterms out sym
    ) prod.right;

  if prod.prec != 0 then
    Printf.fprintf out " %%prec(%d)" prod.prec
