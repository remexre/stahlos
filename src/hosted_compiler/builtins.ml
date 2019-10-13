open Tast

let builtins : (string * expr) list =
  let ty = { type_ = Universe(1); value = Universe(0) } in
  [ "%I64", ty
  ; "%String", ty
  ; "%U64", ty
  ]
