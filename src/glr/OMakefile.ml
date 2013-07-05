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
    "Lexerint";
    "Objpool";
    "Options";
    "PtreeActions";
    "PtreeNode";
    "SemanticValue";
    "SourceLocation";
    "TokenInfo";
    "UserActions";
  ];

  (* Library dependencies *)
  OCamlRequires [
    "baselib";
    "parsetables";
  ];
]
