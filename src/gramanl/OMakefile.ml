install Package ".DEFAULT" [
  (* Target *)
  Name		"gramanl";
  Description	"Grammar analysis library";
  Version	"0.1";

  (* Sources *)
  Modules [
    "AnalysisEnvType";
    "Assoc";
    "BackTransform";
    "BfsTree";
    "ConflictResolution";
    "Derivability";
    "DottedProduction";
    "EmitActions";
    "EmitCode";
    "EmitNames";
    "EmitPtree";
    "EmitTables";
    "EmitTokens";
    "EmitTreematch";
    "FirstSets";
    "FollowSets";
    "GrammarAnalysis";
    "GrammarAst";
    "GrammarGraph";
    "GrammarIndex";
    "GrammarLexer";
    "GrammarParser";
    "GrammarSig";
    "GrammarStructure";
    "GrammarTreeParser";
    "GrammarType";
    "GrammarUtil";
    "Ids";
    "ItemList";
    "ItemSet";
    "LrItem";
    "LrItemSets";
    "Merge";
    "Nonterminal";
    "NonterminalSet";
    "NtArray";
    "NtSet";
    "Options";
    "OutputMenhir";
    "ParseActions";
    "PrintAnalysisEnv";
    "PrintAst";
    "PrintGrammar";
    "ProdArray";
    "PtreeMaker";
    "PtreeStructure";
    "PtreeType";
    "Reachability";
    "Renumbering";
    "SampleInput";
    "Semantic";
    "SemanticVariant";
    "StateGraph";
    "SuperSets";
    "TableConstruction";
    "TableEncoding";
    "TablePrinting";
    "TermArray";
    "Terminal";
    "TerminalSet";
    "TermSet";
    "Warnings";
  ];

  (* Library dependencies *)
  OCamlRequires [
    "baselib";
    "camlp4.fulllib";
    "codegen";
    "parsetables";
    "ocamlgraph";
    "sexplib.syntax";
  ];

  (* Camlp4 *)
  Flags [
    "analysisEnvType.ml",	"-syntax camlp4o";
    "assoc.ml",			"-syntax camlp4o";
    "emitActions.ml",		"-pp camlp4of";
    "emitNames.ml",		"-pp camlp4of";
    "emitPtree.ml",		"-pp camlp4of";
    "emitTables.ml",		"-pp camlp4of";
    "emitTokens.ml",		"-pp camlp4of";
    "grammarAst.ml",		"-syntax camlp4o";
    "grammarType.ml",		"-syntax camlp4o";
    "grammarTreeParser.ml",	"-pp camlp4of";
    "merge.ml",			"-syntax camlp4o";
    "parseActions.ml",		"-pp camlp4of";
    "ptreeMaker.ml",		"-pp camlp4of";
    "ptreeType.ml",		"-syntax camlp4o";
    "semanticVariant.ml",	"-syntax camlp4o";
  ];
]
