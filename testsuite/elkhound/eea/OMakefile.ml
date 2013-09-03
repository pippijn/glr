install Program ".DEFAULT" [
  (* Target *)
  Name		"eea";
  Description	"Highly ambiguous parser E -> E E | a";
  Version	"0.1";

  (* Sources *)
  Modules [
    "Eea";
    "EeaLexer";
  ];

  (* Library dependencies *)
  OCamlRequires [
    "glr";
  ];

  Parser (Elkhound, "eea");
]
