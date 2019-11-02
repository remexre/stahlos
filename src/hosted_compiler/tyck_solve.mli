type constraint_
  = Eq of Uast.expr_inner * Uast.expr_inner
  | Lub of Uast.expr_inner * Uast.expr_inner * Uast.expr_inner
  | Ty of Uast.expr_inner * Uast.expr_inner

val string_of_constraint : constraint_ -> string

exception Failed_to_solve of constraint_
val solve : (string * Tast.expr_inner) list -> constraint_ list -> Uast.subst
