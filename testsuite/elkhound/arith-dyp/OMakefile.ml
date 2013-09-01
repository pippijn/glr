install Program ".DEFAULT" [
  (* Target *)
  Name		"arith-dyp";
  Description	"Arith parser built with Dypgen";
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
    "dyp";
  ];
]
