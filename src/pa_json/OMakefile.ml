install Syntax ".DEFAULT" [
  (* Target *)
  Name		"pa_json";
  Description	"JSON quotations";
  Version	"0.1";

  (* Sources *)
  Modules [
    "Jq_ast";
    "Jq_parser";
    "Jq_quotations";
  ];

  (* Library dependencies *)
  OCamlRequires [
    "camlp4.extend";
    "camlp4.quotations";
  ];

  (* Camlp4 *)
  Flags [
    "jq_ast.ml",	"-syntax camlp4o";
    "jq_parser.ml",	"-pp camlp4of";
    "jq_quotations.ml",	"-pp camlp4of";
  ];
]
