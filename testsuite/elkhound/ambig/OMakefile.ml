install Program ".DEFAULT" [
  (* Target *)
  Name		"ambig";
  Description	"Ambiguous arithmetic expression parser";
  Version	"0.1";

  (* Sources *)
  Modules [
    "Ambig";
    "AmbigAst";
    "AmbigLexer";
  ];

  Flags [
    "ambigAst.ml",	"-syntax camlp4o";
  ];

  Parser (Elkhound, "ambig");
]
