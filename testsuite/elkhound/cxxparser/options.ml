let _ptree = ref false
let _tptree = ref false
let _treematch = ref false
let _print = ref false
let _pp = ref false
let _tokens = ref false
let _dump_toks = ref false
let _load_toks = ref false
let _dump_tree = ref false
let _sizeof_tree = ref false
let _trivial = ref false
let _verbose = ref false
let _xc = ref false
let _rt = ref false


let is_c_source s =
  let length = String.length s in
  if length < 3 then
    false
  else
    s.[length - 2] = '.' &&
    s.[length - 1] = 'c'


let () =
  Cmdline.register "parser" Arg.([
    "-ptree",		Set _ptree,		" build parse tree";
    "-tptree",		Set _tptree,		" build strongly typed parse tree";
    "-treematch",	Set _treematch,		" build treematch-backed tree";
    "-print",		Set _print,		" print tree";
    "-pp",		Set _pp,		" fully tokenise before parsing";
    "-tokens",		Set _tokens,		" tokenise only; do not parse (implies -pp)";
    "-dump-toks",	Set _dump_toks,		" dump tokens to file (implies -pp)";
    "-load-toks",	Set _load_toks,		" load tokens from file";
    "-dump-tree",	Set _dump_tree,		" dump result of the parse to file";
    "-sizeof-tree",	Set _sizeof_tree,	" compute memory size of parse result";
    "-trivial",		Set _trivial,		" use trivial user actions";
    "-verbose",		Set _verbose,		" show each file name as it is processed";
    "-xc",		Set _xc,		" parse code as C, not as C++ (implicit if any input file name ends with .c)";
    "-rt",		Set _rt,		" set real-time scheduling policy with highest priority";
  ]) ~action:(fun inputs ->
    if !_dump_toks || !_tokens then
      _pp := true;

    if List.filter is_c_source inputs <> [] then
      _xc := true;
  )


let _ptree () = !_ptree
let _tptree () = !_tptree
let _treematch () = !_treematch
let _print () = !_print
let _pp () = !_pp
let _tokens () = !_tokens
let _dump_toks () = !_dump_toks
let _load_toks () = !_load_toks
let _dump_tree () = !_dump_tree
let _sizeof_tree () = !_sizeof_tree
let _trivial () = !_trivial
let _verbose () = !_verbose
let _xc () = !_xc
let _rt () = !_rt
