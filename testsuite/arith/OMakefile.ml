install Program ".DEFAULT" [
  (* Target *)
  Name		"arith";
  Description	"Arith parser";
  Version	"0.1";

  (* Sources *)
  Modules [
    "Arith";
    "ArithLexer";
  ];

  (* Library dependencies *)
  OCamlRequires [
    "glr";
  ];

  Parser (Glr, "arith");
]
