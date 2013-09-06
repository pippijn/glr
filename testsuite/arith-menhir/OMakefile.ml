install Program ".DEFAULT" [
  (* Target *)
  Name		"arith-menhir";
  Description	"Arith parser built with Menhir";
  Version	"0.1";

  (* Sources *)
  Modules [
    "Arith";
    "ArithLexer";
    "ArithParser";
  ];

  (* Library dependencies *)
  OCamlRequires [
    "corelib";
    "menhirLib";
  ];

  (*Code "MENHIR_FLAGS += -t";*)
]
