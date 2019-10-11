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

exception Type_error of expr * expr

type ctx_entry =
  { type_ : Tast.expr
  ; value : Tast.expr option
  }
type ctx = (string * ctx_entry) list

val tyck_module : Ast.module_ -> Tast.module_
