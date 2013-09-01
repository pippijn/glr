(* turn this on to detect cyclicity; there is a performance penalty *)
let _ptree_cycles	= ref true
let _ptree_indent	= ref 1

let _dup_clone		= ref false


let () =
  Cmdline.register "parse tree" (Arg.([
    "-ptree-cycles",		Set _ptree_cycles,		" detect parse tree cyclicity";
    "-ptree-indent",		Set_int _ptree_indent,		"<amount> indentation per level in parse tree printing";
    "-dup-clone",		Set _dup_clone,			" make default dup function perform a deep copy";
  ]))


let _ptree_cycles	() = !_ptree_cycles
let _ptree_indent	() = !_ptree_indent
let _dup_clone		() = !_dup_clone
