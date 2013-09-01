install Program ".DEFAULT" [
  (* Target *)
  Name		"arith-bison";
  Description	"Arith parser built with Bison";
  Version	"0.1";

  (* Sources *)
  Sources [
    "arith.c";
    "arithLexer.l";
    "arithParser.y";
  ];

  Headers [
    "arith.h";
    "arithParser.h";
  ];

  Var ("OM_CFLAGS", "-O2");
]
