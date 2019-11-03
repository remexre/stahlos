type expr =
  { type_ : expr_inner
  ; value : expr_inner
  }

and expr_inner
  = App of expr * expr
  | Global of string
  | Lam of string * expr
  | Lit of Sexpr.t
  | LitTy of string
  | Local of int
  | Pi of string * expr * expr
  | Universe
  | Var of string * int

type subst = (int * expr_inner) list

val walk_inner : subst -> expr_inner -> expr_inner

val from_nast : Nast.expr -> expr_inner

val from_tast_inner : Tast.expr_inner -> expr_inner

exception Unsolved_variable of string * int * expr list
val tast_of_expr : subst -> expr -> Tast.expr
val tast_of_expr_inner : subst -> expr_inner -> Tast.expr_inner

val string_of_expr : expr -> string
val string_of_expr_inner : expr_inner -> string
