install Program ".DEFAULT" [
  (* Target *)
  Name		"sless";
  Description	"Scannerless parser";
  Version	"0.1";

  (* Sources *)
  Modules [
    "Sless";
    "SlessLexer";
  ];

  (* Library dependencies *)
  OCamlRequires [
    "glr";
  ];

  Parser (Elkhound, "sless");
]
