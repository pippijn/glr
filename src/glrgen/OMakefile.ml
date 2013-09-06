install Program ".DEFAULT" [
  (* Target *)
  Name		"glrgen";

  (* Sources *)
  Modules [
    "Glrgen";
  ];

  (* Library dependencies *)
  OCamlRequires [
    "gramanl";
  ];
]
