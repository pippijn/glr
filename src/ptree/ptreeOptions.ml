(* turn this on to detect cyclicity; there is a performance penalty *)
let _ptree_cycles	= ref false
let _ptree_indent	= ref 1


let () =
  Cmdline.register "parse tree" (Arg.([
    "-ptree-cycles",		Set _ptree_cycles,		" detect parse tree cyclicity";
    "-ptree-indent",		Set_int _ptree_indent,		"<amount> indentation per level in parse tree printing";
  ]))


let _ptree_cycles	() = !_ptree_cycles
let _ptree_indent	() = !_ptree_indent
