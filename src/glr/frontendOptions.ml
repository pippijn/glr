let _ptree	= ref false
let _tptree	= ref false
let _treematch	= ref false
let _useract	= ref false

let _print	= ref false
let _pp		= ref false
let _tokens	= ref false
let _dump_toks	= ref false
let _load_toks	= ref false
let _dump_tree	= ref false
let _trivial	= ref false
let _verbose	= ref false
let _lrparse	= ref false


let () =
  Cmdline.register "frontend" Arg.([
    "-ptree",		Set _ptree,		" build parse tree";
    "-tptree",		Set _tptree,		" build strongly typed parse tree";
    "-treematch",	Set _treematch,		" build treematch-backed tree";
    "-useract",		Set _useract,		" perform semantic actions";

    "-print",		Set _print,		" print tree";
    "-pp",		Set _pp,		" fully tokenise before parsing";
    "-tokens",		Set _tokens,		" tokenise only; do not parse (implies -pp)";
    "-dump-toks",	Set _dump_toks,		" dump tokens to file (implies -pp)";
    "-load-toks",	Set _load_toks,		" load tokens from file";
    "-dump-tree",	Set _dump_tree,		" dump result of the parse to file";
    "-trivial",		Set _trivial,		" use trivial user actions";
    "-verbose",		Set _verbose,		" show each file name as it is processed";
    "-lrparse",		Set _lrparse,		" use LR core instead of GLR";
  ]) ~action:(fun inputs ->
    if !_dump_toks || !_tokens then
      _pp := true;
  )


let _ptree	() = !_ptree
let _tptree	() = !_tptree
let _treematch	() = !_treematch
let _useract	() = !_useract

let _print	() = !_print
let _pp		() = !_pp
let _tokens	() = !_tokens
let _dump_toks	() = !_dump_toks
let _load_toks	() = !_load_toks
let _dump_tree	() = !_dump_tree
let _trivial	() = !_trivial
let _verbose	() = !_verbose
let _lrparse	() = !_lrparse
