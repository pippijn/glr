let print_table ?(cols=14) out table =
  Printf.fprintf out "[| (* %d elements *)" (Array.length table);
  Array.iteri (fun i value ->
    (* this loop is optimised not to use printf
     * the rest of the printing code is not time-critical *)
    if i mod cols = 0 then (
      Printf.fprintf out "\n    (*%4d*) " (i / cols);
    );
    output_string out (string_of_int value);
    output_string out "; ";
  ) table;
  Printf.fprintf out "\n  |]"


let print_tables tables out =
  let open ParseTablesType in

  Printf.fprintf out "let parseTables = ParseTablesType.({\n";
  Printf.fprintf out "  numTerms = %d;\n" tables.numTerms;
  Printf.fprintf out "  numNonterms = %d;\n" tables.numNonterms;
  Printf.fprintf out "  numProds = %d;\n" tables.numProds;
  Printf.fprintf out "\n";
  Printf.fprintf out "  numStates = %d;\n" tables.numStates;
  Printf.fprintf out "\n";
  Printf.fprintf out "  actionCols = %d;\n" tables.actionCols;
  Printf.fprintf out "  actionTable = ";
  print_table ~cols:tables.actionCols out tables.actionTable;
  Printf.fprintf out ";\n";
  Printf.fprintf out "\n";
  Printf.fprintf out "  gotoCols = %d;\n" tables.gotoCols;
  Printf.fprintf out "  gotoTable = ";
  print_table ~cols:tables.gotoCols out tables.gotoTable;
  Printf.fprintf out ";\n";
  Printf.fprintf out "\n";
  Printf.fprintf out "  prodInfo_rhsLen = ";
  print_table out tables.prodInfo_rhsLen;
  Printf.fprintf out ";\n";
  Printf.fprintf out "\n";
  Printf.fprintf out "  prodInfo_lhsIndex = ";
  print_table out tables.prodInfo_lhsIndex;
  Printf.fprintf out ";\n";
  Printf.fprintf out "\n";
  Printf.fprintf out "  stateSymbol = ";
  print_table out tables.stateSymbol;
  Printf.fprintf out ";\n";
  Printf.fprintf out "\n";
  Printf.fprintf out "  ambigTable = ";
  print_table out tables.ambigTable;
  Printf.fprintf out ";\n";
  Printf.fprintf out "\n";
  Printf.fprintf out "  nontermOrder = ";
  print_table out tables.nontermOrder;
  Printf.fprintf out ";\n";
  Printf.fprintf out "\n";
  Printf.fprintf out "  startState = %d;\n" tables.startState;
  Printf.fprintf out "  finalProductionIndex = %d;\n" tables.finalProductionIndex;
  Printf.fprintf out "})\n"
