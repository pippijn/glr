install Package ".DEFAULT" [
  (* Target *)
  Name		"glr";
  Description	"Generalised LR parsing engine";
  Version	"0.1";

  (* Sources *)
  Modules [
    "Arraystack";
    "Easy";
    "Engine";
    "Objpool";
    "Options";
  ];

  (* Library dependencies *)
  OCamlRequires [
    "corelib";
    "parsetables";
  ];
]
