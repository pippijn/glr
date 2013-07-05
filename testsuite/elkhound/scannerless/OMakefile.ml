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

  Parser (Elkhound, "sless");
]
