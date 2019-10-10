open Ast

type builtin =
  { type_ : expr
  }

let builtins : (string * builtin) list =
  [ "%Sint64", { type_ = Universe }
  ; "%String", { type_ = Universe }
  ; "%Uint64", { type_ = Universe }
  ]
