install Library ".DEFAULT" [
  (* Target *)
  Name		"parsetables";
  Description	"Common modules for parser generator and engine";
  Version	"0.1";

  (* Sources *)
  Modules [
    "ActionOptions";
    "Lexerint";
    "Liststack";
    "ParseTables";
    "ParseTablesType";
    "SemanticValue";
    "SourceLocation";
    "TokenInfo";
    "UserActions";
  ];

  (* Library dependencies *)
  OCamlRequires [
    "corelib";
  ];
]
