let _terminal_names	= ref false
let _trace_parse	= ref false
let _accounting		= ref false
let _use_mini_lr	= ref true

(* turn this on to detect cyclicity; there is a performance penalty *)
let _ptree_cycles	= ref true
let _ptree_indent	= ref 2


let () =
  Cmdline.register "glr engine" (Arg.([
    "-terminal-names",		Set _terminal_names,		" display terminal names instead of their aliases";
    "-trace-parse",		Set _trace_parse,		" trace parse actions in GLR engine";
    "-accounting",		Set _accounting,		" keep some statistics useful for performance evaluation";
    "-use-mini-lr",		Set _use_mini_lr,		" use the mini LR core";
    "-ptree-cycles",		Set _ptree_cycles,		" detect parse tree cyclicity";
    "-ptree-indent",		Set_int _ptree_indent,		"<amount> indentation per level in parse tree printing";
  ]))


let _terminal_names	() = !_terminal_names
let _trace_parse	() = !_trace_parse
let _accounting		() = !_accounting
let _use_mini_lr	() = !_use_mini_lr
let _ptree_cycles	() = !_ptree_cycles
let _ptree_indent	() = !_ptree_indent
