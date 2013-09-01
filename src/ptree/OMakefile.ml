install Library ".DEFAULT" [
  (* Target *)
  Name		"ptree";
  Description	"Untyped parse tree actions";
  Version	"0.1";

  (* Sources *)
  Modules [
    "PtreeActions";
    "PtreeNode";
    "PtreeOptions";
  ];

  (* Library dependencies *)
  OCamlRequires [
    "parsetables";
  ];
]
