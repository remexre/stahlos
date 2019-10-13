type cstr
  = Eq of Uast.expr_inner * Uast.expr_inner
  | Ty of Uast.expr_inner * Uast.expr_inner

val string_of_cstr : cstr -> string

exception Failed_to_solve of cstr
exception Unsolved_variable of int

val tyck_module : Ast.module_ -> Tast.module_
