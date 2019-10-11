type expr =
  { type_ : expr_inner
  ; value : expr_inner
  }

and expr_inner
  = App of expr * expr
  | Global of string
  | Lam of expr
  | Lit of string * Sexpr.t
  | Logic of int
  | Local of int
  | Pi of expr * expr
  | Universe of int

val from_ast : Ast.def list -> Ast.expr -> expr  
