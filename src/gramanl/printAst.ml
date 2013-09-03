open GrammarAst

let f = Printf.fprintf


let print_sloc out located =
  output_string out (Sloc.value located)


let print_termdecl out = function
  | TermDecl (code, name, None) ->
      assert (Sloc.value name <> "");
      f out "  %3d : %a;\n" code print_sloc name
  | TermDecl (code, name, Some alias) ->
      assert (Sloc.value name <> "");
      assert (Sloc.value alias <> "");
      f out "  %3d : %a \"%a\";\n" code print_sloc name print_sloc alias

let print_action_code out = function
  | None ->
      f out ";\n"
  | Some code ->
      f out " { %a }\n" print_sloc code

let print_specfunc out = function
  | SpecFunc (name, formals, code) ->
      assert (Sloc.value name <> "");
      f out "  fun %a (" print_sloc name;
      ignore (List.fold_left (fun first formal ->
        if not first then
          output_string out ", ";
        print_sloc out formal;
        false
      ) true formals);
      f out ")%a" print_action_code (Some code)

let print_type out = function
  | None -> ()
  | Some ty ->
      assert (Sloc.value ty <> "");
      f out "(%a)" print_sloc ty

let print_termtype out = function
  | TermType (name, termtype, []) ->
      assert (Sloc.value name <> "");
      f out "  token%a %a;\n" print_type (Some termtype) print_sloc name
  | TermType (name, termtype, funcs) ->
      assert (Sloc.value name <> "");
      f out "  token%a %a {\n" print_type (Some termtype) print_sloc name;
      List.iter (print_specfunc out) funcs;
      f out "}\n"

let print_precspec out = function
  | PrecSpec (kind, prec, tokens) ->
      f out "    %s %d" (Assoc.to_string (Sloc.value kind)) prec;
      List.iter (f out " %a" print_sloc) tokens;
      f out ";\n"

let print_tag out = function
  | None -> ()
  | Some tag ->
      assert (Sloc.value tag <> "");
      f out "%a:" print_sloc tag

let print_rhs out = function
  | RH_name (tag, name) ->
      assert (Sloc.value name <> "");
      f out " %a%a" print_tag tag print_sloc name
  | RH_string (tag, str) ->
      assert (Sloc.value str <> "");
      f out " %a\"%a\"" print_tag tag print_sloc str
  | RH_prec (tokName) ->
      assert (Sloc.value tokName <> "");
      f out " prec (%a)" print_sloc tokName
  | RH_forbid (tokName) ->
      assert (Sloc.value tokName <> "");
      f out " forbid_next (%a)" print_sloc tokName

let print_prod_name out = function
  | None -> ()
  | Some name ->
      assert (Sloc.value name <> "");
      f out " [%a]" print_sloc name

let print_rhs out = function
  | [] ->
      f out " empty"
  | rhs ->
      List.iter (print_rhs out) rhs

let print_proddecl out = function
  | ProdDecl (PDK_NEW, prod_name, rhs, actionCode) ->
      f out "  ->";
      print_rhs out rhs;
      print_prod_name out prod_name;
      print_action_code out actionCode
  | ProdDecl (PDK_REPLACE, None, rhs, actionCode) ->
      f out "  replace";
      print_rhs out rhs;
      print_action_code out actionCode
  | ProdDecl (PDK_DELETE, None, rhs, actionCode) ->
      f out "  delete";
      print_rhs out rhs;
      print_action_code out actionCode
  | _ -> failwith "invalid ProdDecl"

let print_topform out = function
  | TF_verbatim (false, code) ->
      f out "\nverbatim {\n%a\n}\n" print_sloc code
  | TF_verbatim (true, code) ->
      f out "\nimpl_verbatim {\n%a\n}\n" print_sloc code
  | TF_option (name, value) ->
      f out "option %a %d;\n" print_sloc name value
  | TF_terminals (decls, types, prec) ->
      f out "\nterminals {\n";
      List.iter (print_termdecl out) decls;
      f out "\n";
      List.iter (print_termtype out) types;
      f out "\n  precedence {\n";
      List.iter (print_precspec out) prec;
      f out "  }\n";
      f out "}\n\n"
  | TF_nonterm (name, semtype, funcs, prods, subsets) ->
      assert (Sloc.value name <> "");
      f out "nonterm%a %a {\n" print_type semtype print_sloc name;
      List.iter (print_specfunc out) funcs;
      List.iter (print_proddecl out) prods;
      List.iter (f out "  %a\n" print_sloc) subsets;
      f out "}\n\n"

let print ?(out=stdout) ast =
  List.iter (print_topform out) ast
