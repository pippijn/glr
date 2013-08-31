install Program ".DEFAULT" [
  (* Target *)
  Name		"cxxparser";
  Description	"C++ parser";
  Version	"0.1";

  (* Sources *)
  Modules [
    "Cc";
    "CcLexer";
    "Cc_keywords";
    "Options";
  ];

  (* Library dependencies *)
  OCamlRequires [
    "glr";
  ];

  Code "ccParser.ml: ccTerminals.ids";

  Parser (Elkhound, "cc");
]
