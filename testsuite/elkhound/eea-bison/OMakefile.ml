install Program ".DEFAULT" [
  (* Target *)
  Name		"eea-bison";
  Description	"Arith parser built with Bison";
  Version	"0.1";

  (* Sources *)
  Sources [
    "eea.c";
    "eeaLexer.l";
    "eeaParser.y";
  ];

  Headers [
    "eea.h";
    "eeaParser.h";
  ];

  Var ("OM_CFLAGS", "-O2");
]
