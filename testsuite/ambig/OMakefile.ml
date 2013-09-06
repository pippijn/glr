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

  (* Library dependencies *)
  OCamlRequires [
    "glr";
  ];

  Flags [
    "ambigAst.ml",	"-syntax camlp4o";
  ];

  Parser (Glr, "ambig");
]
