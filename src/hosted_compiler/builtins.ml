open Tast

let builtins : (string * expr) list =
  let ty = { type_ = Universe(0); value = Universe(1) } in
  [ "%I64", ty
  ; "%String", ty
  ; "%U64", ty
  ]
