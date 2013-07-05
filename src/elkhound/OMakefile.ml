install Program ".DEFAULT" [
  (* Target *)
  Name		"elkhound";

  (* Sources *)
  Modules [
    "Elkhound";
  ];

  (* Library dependencies *)
  OCamlRequires [
    "gramanl";
  ];
]
