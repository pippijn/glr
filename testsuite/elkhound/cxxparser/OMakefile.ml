install Program ".DEFAULT" [
  (* Target *)
  Name		"cxxparser";
  Description	"C++ parser";
  Version	"0.1";

  (* Sources *)
  Modules [
    "Cc";
    "CcLexer";
  ];

  (* Library dependencies *)
  OCamlRequires [
    "glr";
  ];

  Parser (Elkhound, "cc");
]
