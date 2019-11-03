open Tast

let builtins : (string * expr_inner) list =
  [ "%I64", Universe
  ; "%String", Universe
  ; "%U64", Universe
  ]
