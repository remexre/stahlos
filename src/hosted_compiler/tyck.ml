open Tyck_ctx
open Tyck_solve
open Utils

let from_ast (defs: Ast.def list) : int -> Ast.expr -> Uast.expr_inner Tyck_ctx.t =
  let rec to_expr_inner (given: int) : Ast.expr -> Uast.expr_inner Tyck_ctx.t = function
  | App(f, x) ->
      let+ f = to_expr 0 f
      and+ x = to_expr 0 x
      in return (Uast.App(f, x))
  | AppI(f, x) ->
      let+ f = to_expr (given+1) f
      and+ x = to_expr 0 x
      in return (Uast.App(f, x))
  | Global(s) -> let _ = (defs, given) in return (Uast.Global(s)) (* TODO *)
  | Hole -> fresh
  | Lam(n, b) | LamI(n, b) ->
      let+ b = to_expr 0 b
      in return (Uast.Lam(n, b))
  | Lit(l) -> return (Uast.Lit(l))
  | Local(n) -> return (Uast.Local(n))
  | Pi(n, t, b) | PiI(n, t, b) ->
      let+ t = to_expr 0 t
      and+ b = to_expr 0 b
      in return (Uast.Pi(n, t, b))
  | Universe -> return (Uast.Universe(0))
  and to_expr (given: int) (expr: Ast.expr) : Uast.expr Tyck_ctx.t =
    let+ expr = to_expr_inner given expr
    and+ ty = fresh
    in return { Uast.type_ = ty; Uast.value = expr }
  in to_expr_inner

let tyck_expr (expr: Ast.expr) (ty: Tast.expr_inner) : Tast.expr Tyck_ctx.t =
  let+ ctx = get in
  let ty = Uast.from_tast ty in
  let+ expr = from_ast ctx.ast_defs 0 expr in
  modify (fun ctx -> { ctx with subst = [(0, expr); (1, ty)] }) >>
  ctx_add_cstr_ty (Logic(0)) (Logic(1)) >>
  ctx_finish

let add_ctor (dt: Ast.defty) (c: Ast.ctor) : unit Tyck_ctx.t =
  let _ = (dt, c) in
  (*
  return "TODO add_ctor" >>= failwith
  *)
  return ()

let add_defty (dt: Ast.defty) : unit Tyck_ctx.t =
  if dt.pargs = [] && dt.iargs = [] then
  assert (dt.pargs = []);
  assert (dt.iargs = []);
  ctx_add_def dt.name { type_ = Tast.Universe(1); value = Tast.Universe(0) }

let add_elim (dt: Ast.defty) : unit Tyck_ctx.t =
  let _ = dt in
  (*
  return "TODO add_elim" >>= failwith
  *)
  return ()

let tyck_def (def: Ast.def) : unit Tyck_ctx.t =
  return ("tyck_def " ^ Ast.string_of_def def) >>= (return %% prerr_endline) >>
  begin
    match def with
    | Ast.Def(n, t, e) ->
        let+ t = tyck_expr t (Tast.Universe(0)) in
        let+ e = tyck_expr e t.value in
        ctx_add_def n e
    | Ast.Deftype(dt, cs) ->
        add_defty dt >>
        add_elim dt >>
        forM_ cs (add_ctor dt)
  end >>
  return "===========================" >>= (return %% prerr_endline)

let tyck_module' (m: Ast.module_) : unit Tyck_ctx.t =
  forM_ m.defs (fun def -> tyck_def def >> ctx_add_ast_def def)

let tyck_module (m: Ast.module_) : Tast.module_ =
  let ctx = exec ctx_empty (tyck_module' m) in
  { name = m.name; defs = ctx.defs }
