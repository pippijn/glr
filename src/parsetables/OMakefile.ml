install Library ".DEFAULT" [
  (* Target *)
  Name		"parsetables";
  Description	"Common modules for parser generator and engine";
  Version	"0.1";

  (* Sources *)
  Modules [
    "ParseTables";
    "ParseTablesType";
  ];
]