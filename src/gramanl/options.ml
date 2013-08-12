let _module_prefix		= ref ""

let _paranoid			= ref false

let _use_LR0			= ref false
let _use_SLR1			= ref false
let _use_LR1			= ref false
let _use_LALR1			= ref true

let _graph_grammar		= ref true
let _graph_automaton		= ref false
let _dump_automaton		= ref true

let _print_merged		= ref false
let _print_transformed		= ref true
let _output_menhir		= ref true

let _gen_table_text		= ref false
let _use_table_dump		= ref true
let _inline_table_dump		= ref true
let _compress_table_dump	= ref false

let _optimise			= ref false

let _trace_lexing		= ref false
let _trace_renumbering		= ref false
let _trace_closure		= ref false
let _trace_lrsets		= ref false
let _trace_prec			= ref false
let _trace_conflict		= ref false
let _trace_table		= ref false
let _trace_reductions		= ref false
let _trace_first		= ref false
let _trace_derivable		= ref false
let _trace_merge		= ref false
let _trace_rewrite		= ref true
let _trace_unreachable_ptree	= ref false


let () =
  Cmdline.register "grammar analysis" Arg.([
    "-module-prefix",		Set_string _module_prefix,	" prefix for generated modules (%Tables, %Actions, ...)";

    "-paranoid",		Set _paranoid,			" do more expensive internal checks";

    "-use-lr0",			Set _use_LR0,			" generate an LR(0) automaton";
    "-use-slr1",		Set _use_SLR1,			" generate an SLR(1) automaton";
    "-use-lr1",			Set _use_LR1,			" generate an LR(1) automaton";
    "-use-lalr1",		Set _use_LALR1,			" generate an LALR(1) automaton";

    "-graph-grammar",		Set _graph_grammar,		" visualise grammar as graph";
    "-graph-automaton",		Set _graph_automaton,		" visualise LR automaton as graph";
    "-dump-automaton",		Set _dump_automaton,		" dump automaton to a file after LR item set construction";

    "-print-merged",		Set _print_merged,		" print combined grammar";
    "-print-transformed",	Set _print_transformed,		" regenerate and print grammar after all transformations";
    "-output-menhir",		Set _output_menhir,		" output combined grammar in Menhir format";

    "-gen-table-text",		Set _gen_table_text,		" generate tables using more efficient algorithm";
    "-use-table-dump",		Set _use_table_dump,		" load serialised tables instead of emitting arrays in code";
    "-inline-table-dump",	Set _inline_table_dump,		" inline serialised tables into the Tables module as string";
    "-compress-table-dump",	Set _compress_table_dump,	" compress serialised tables with zlib";

    "-optimise",		Set _optimise,			" perform various optimisations on the grammar and ptree";

    "-trace-lexing",		Set _trace_lexing,		" output each token as it is parsed";
    "-trace-renumbering",	Set _trace_renumbering,		" output details on state renumbering";
    "-trace-closure",		Set _trace_closure,		" output details during LR item set closure";
    "-trace-lrsets",		Set _trace_lrsets,		" output details during LR item set construction";
    "-trace-prec",		Set _trace_prec,		" output details when precedences are used to resolve conflicts";
    "-trace-conflict",		Set _trace_conflict,		" output details during conflict resolution";
    "-trace-table",		Set _trace_table,		" output details during parse table construction";
    "-trace-reductions",	Set _trace_reductions,		" output details on possible reductions for each state";
    "-trace-first",		Set _trace_first,		" output details during First-set computation";
    "-trace-derivable",		Set _trace_derivable,		" output details during derivability relation computation";
    "-trace-merge",		Set _trace_merge,		" output details while merging grammar modules";
    "-trace-rewrite",		Set _trace_rewrite,		" output details during symbolic reduction";
    "-trace-unreachable-ptree",	Set _trace_unreachable_ptree,	" show which parse tree nodes are removed due to unreachability";
  ]) ~action:(fun inputs ->
    if !_module_prefix = "" then (
      let name = List.hd inputs in

      let slash = String.rindex name '/' + 1 in
      let point = String.rindex name '.' in
      let name = String.sub name slash (point - slash) in

      _module_prefix := String.capitalize name
    )
  )


let _module_prefix		() = !_module_prefix

let _paranoid			() = !_paranoid

let _use_LR0			() = !_use_LR0
let _use_SLR1			() = !_use_SLR1
let _use_LR1			() = !_use_LR1
let _use_LALR1			() = !_use_LALR1

let _graph_grammar		() = !_graph_grammar
let _graph_automaton		() = !_graph_automaton
let _dump_automaton		() = !_dump_automaton

let _print_merged		() = !_print_merged
let _print_transformed		() = !_print_transformed
let _output_menhir		() = !_output_menhir

let _gen_table_text		() = !_gen_table_text
let _use_table_dump		() = !_use_table_dump
let _inline_table_dump		() = !_inline_table_dump
let _compress_table_dump	() = !_compress_table_dump

let _optimise			() = !_optimise

let _trace_lexing		() = !_trace_lexing
let _trace_renumbering		() = !_trace_renumbering
let _trace_closure		() = !_trace_closure
let _trace_lrsets		() = !_trace_lrsets
let _trace_prec			() = !_trace_prec
let _trace_conflict		() = !_trace_conflict
let _trace_table		() = !_trace_table
let _trace_reductions		() = !_trace_reductions
let _trace_first		() = !_trace_first
let _trace_derivable		() = !_trace_derivable
let _trace_merge		() = !_trace_merge
let _trace_rewrite		() = !_trace_rewrite
let _trace_unreachable_ptree	() = !_trace_unreachable_ptree
