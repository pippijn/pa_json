install Library ".DEFAULT" [
  (* Target *)
  Name		"json";
  Description	"JSON parsing library";
  Version	"0.1";

  (* Sources *)
  Modules [
    "Basic";
    "Lexer";
    "Olexer";
    "Parser";
  ];
]