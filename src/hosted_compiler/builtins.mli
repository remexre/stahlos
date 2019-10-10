type builtin =
  { type_ : Ast.expr
  }

val builtins : (string * builtin) list
