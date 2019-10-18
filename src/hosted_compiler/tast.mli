type expr =
  { type_ : expr_inner
  ; value : expr_inner
  }

and expr_inner
  = App of expr * expr
  | Global of string
  | Lam of string * expr
  | Lit of Sexpr.t
  | LitTy
  | Local of int
  | Pi of string * expr * expr
  | Universe of int

val string_of_expr_inner : expr_inner -> string
val string_of_expr : expr -> string

type module_ =
  { name : string
  ; defs : (string * expr * int) list
  }

val string_of_module : module_ -> string
