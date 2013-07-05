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

  Parser (Elkhound, "arith");
]
