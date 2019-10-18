type cstr
  = Eq of Uast.expr_inner * Uast.expr_inner
  | Ty of Uast.expr_inner * Uast.expr_inner

let string_of_cstr = function
  | Eq(l, r) -> Uast.string_of_expr_inner l ^ " = " ^ Uast.string_of_expr_inner r
  | Ty(l, r) -> Uast.string_of_expr_inner l ^ " : " ^ Uast.string_of_expr_inner r

exception Failed_to_solve of cstr
exception Unsolved_variable of int * Uast.expr_inner list

type ctx =
  { ast_defs : Ast.def list
  ; cstrs : cstr list
  ; defs : (string * Tast.expr) list
  ; fresh : int
  ; subst : (int * Uast.expr_inner) list
  }

include Monad.State_pure(struct type t = ctx end)

let fresh =
  let+ ctx = get in
  modify (fun ctx -> { ctx with fresh = ctx.fresh + 1 }) >>
  return (Uast.Logic(ctx.fresh))

let ctx_empty = { ast_defs = []; cstrs = []; defs = Builtins.builtins
                ; fresh = 2; subst = [] }

let ctx_add_ast_def def =
  modify (fun ctx -> { ctx with ast_defs = def::ctx.ast_defs })

let ctx_add_cstr_eq l r =
  modify (fun ctx -> { ctx with cstrs = Eq(l, r)::ctx.cstrs })

let ctx_add_cstr_ty l r =
  modify (fun ctx -> { ctx with cstrs = Ty(l, r)::ctx.cstrs })

let ctx_add_cstr_eq_expr l r =
  let open Uast in
  ctx_add_cstr_eq l.type_ r.type_ >>
  ctx_add_cstr_eq l.value r.value

let ctx_add_cstr_ty_expr l r =
  let open Uast in
  ctx_add_cstr_ty l.type_ r.type_ >>
  ctx_add_cstr_ty l.value r.value

let ctx_add_def name expr =
  let open Tast in
  prerr_endline ("add_def (def " ^ name ^ " " ^ Tast.string_of_expr_inner expr.type_ ^
                 " " ^ Tast.string_of_expr expr ^ ")");
  modify (fun ctx -> { ctx with defs = (name, expr)::ctx.defs })

let ctx_ensure cstr cond =
  if cond then return () else raise (Failed_to_solve(cstr))

let ctx_extend var expr =
  modify (fun ctx -> { ctx with subst = (var, expr)::ctx.subst })
