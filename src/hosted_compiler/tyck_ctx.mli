type cstr
  = Eq of Uast.expr_inner * Uast.expr_inner
  | Ty of Uast.expr_inner * Uast.expr_inner

val string_of_cstr : cstr -> string

exception Failed_to_solve of cstr
exception Unsolved_variable of int

type ctx =
  { ast_defs : Ast.def list
  ; cstrs : cstr list
  ; defs : (string * Tast.expr) list
  ; fresh : int
  ; subst : (int * Uast.expr_inner) list
  }

type 'a t

include Monad.Fresh with type f := Uast.expr_inner
                    with type 'a t := 'a t

include Monad.State with type s := ctx
                    with type 'a t := 'a t

val exec : ctx -> unit t -> ctx

val ctx_empty : ctx
val ctx_add_ast_def : Ast.def -> unit t

val ctx_add_cstr_eq : Uast.expr_inner -> Uast.expr_inner -> unit t
val ctx_add_cstr_ty : Uast.expr_inner -> Uast.expr_inner -> unit t
val ctx_add_cstr_eq_expr : Uast.expr -> Uast.expr -> unit t
val ctx_add_cstr_ty_expr : Uast.expr -> Uast.expr -> unit t

val ctx_add_def : string -> Tast.expr -> unit t

val ctx_ensure : cstr -> bool -> unit t

val ctx_extend : int -> Uast.expr_inner -> unit t
