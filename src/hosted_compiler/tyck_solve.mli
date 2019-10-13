val walk : (int * Uast.expr_inner) list -> int -> Tast.expr_inner
val solve_one : Tyck_ctx.cstr -> unit Tyck_ctx.t
val solve_all : unit -> unit Tyck_ctx.t
val ctx_finish : Tast.expr Tyck_ctx.t
