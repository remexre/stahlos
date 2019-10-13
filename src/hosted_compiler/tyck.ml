module State = Monad.State_pure
open State
open Utils

type cstr
  = Eq of Uast.expr_inner * Uast.expr_inner
  | Ty of Uast.expr_inner * Uast.expr_inner

let string_of_cstr : cstr -> string = function
  | Eq(l, r) -> Uast.string_of_expr_inner l ^ " = " ^ Uast.string_of_expr_inner r
  | Ty(l, r) -> Uast.string_of_expr_inner l ^ " : " ^ Uast.string_of_expr_inner r

exception Failed_to_solve of cstr
exception Unsolved_variable of int

type ctx =
  { ast_defs : Ast.def list
  ; cstrs : cstr list
  ; defs : (string * Tast.expr) list
  ; fresh : int
  ; subst : (int * Uast.expr_inner) list
  }

let ctx_empty : ctx = { ast_defs = []; cstrs = []; defs = Builtins.builtins
                      ; fresh = 2; subst = [] }

let ctx_add_ast_def (def: Ast.def) : (ctx, unit) State.t =
  modify (fun ctx -> { ctx with ast_defs = def::ctx.ast_defs })

let ctx_add_cstr_eq (l: Uast.expr_inner) (r: Uast.expr_inner) : (ctx, unit) State.t =
  modify (fun ctx -> { ctx with cstrs = Eq(l, r)::ctx.cstrs })

let ctx_add_cstr_ty (l: Uast.expr_inner) (r: Uast.expr_inner) : (ctx, unit) State.t =
  modify (fun ctx -> { ctx with cstrs = Ty(l, r)::ctx.cstrs })

let ctx_add_cstr_eq_expr (l: Uast.expr) (r: Uast.expr) : (ctx, unit) State.t =
  ctx_add_cstr_eq l.type_ r.type_ >>
  ctx_add_cstr_eq l.value r.value

(*
let ctx_add_cstr_ty_expr (l: Uast.expr) (r: Uast.expr) : (ctx, unit) State.t =
  ctx_add_cstr_ty l.type_ r.type_ >>
  ctx_add_cstr_ty l.value r.value
*)

let ctx_add_def (name: string) (expr: Tast.expr) : (ctx, unit) State.t =
  prerr_endline ("add_def (def " ^ name ^ " " ^ Tast.string_of_expr_inner expr.type_ ^
                 " " ^ Tast.string_of_expr expr ^ ")");
  modify (fun ctx -> { ctx with defs = (name, expr)::ctx.defs })

let ctx_ensure (cstr: cstr) (cond: bool) : (ctx, unit) State.t =
  if cond then return () else raise (Failed_to_solve(cstr))

let ctx_extend (var: int) (expr: Uast.expr_inner) : (ctx, unit) State.t =
  modify (fun ctx -> { ctx with subst = (var, expr)::ctx.subst })

let walk (subst: (int * Uast.expr_inner) list) : int -> Tast.expr_inner =
  let rec lookup (n: int) : Tast.expr_inner =
    match List.assoc_opt n subst with
    | Some(e) -> walk_expr_inner e
    | None -> raise (Unsolved_variable(n))
  and walk_expr (expr: Uast.expr) : Tast.expr =
    { type_ = walk_expr_inner expr.type_; value = walk_expr_inner expr.value }
  and walk_expr_inner (expr: Uast.expr_inner) : Tast.expr_inner =
    match expr with
    | App(f, x) -> let _ = (f, x) in failwith ("TODO walk_expr_inner " ^ Uast.string_of_expr_inner expr)
    | Global(s) -> Tast.Global(s)
    | Lam(n, b) -> let _ = (n, b) in failwith ("TODO walk_expr_inner " ^ Uast.string_of_expr_inner expr)
    | Lit(l) -> let _ = l in failwith ("TODO walk_expr_inner " ^ Uast.string_of_expr_inner expr)
    | LitTy -> Tast.LitTy
    | Local(n) -> Tast.Local(n)
    | Logic(n) -> lookup n
    | Pi(n, t, b) -> Tast.Pi(n, walk_expr t, walk_expr b)
    | Universe(n) -> Tast.Universe(n)
  in lookup

let ctx_finish : (ctx, Tast.expr) State.t =
  let solve_one (cstr: cstr) : (ctx, unit) State.t =
    return ("solve_one " ^ string_of_cstr cstr) >>= (return %% prerr_endline) >>
    match cstr with
    (* Variable-involving cases *)
    | Eq(Logic(l), Logic(r)) when l = r -> return ()
    | Eq(Logic(l), r) -> ctx_extend l r
    | Eq(l, Logic(r)) -> ctx_extend r l
    | Ty(Logic(l), Logic(r)) when l = r -> return ()
    | Ty(Logic(l), r) ->
        begin
          let+ ctx = get in
          match List.assoc_opt l ctx.subst with
          | Some(l) -> ctx_add_cstr_ty l r
          | None -> raise (Failed_to_solve(cstr))
        end
    (* Equality relation *)
    | Eq(App(lf, lx), App(rf, rx)) -> ctx_add_cstr_eq_expr lf rf >> ctx_add_cstr_eq_expr lx rx
    | Eq(Global(l), Global(r)) -> ctx_ensure cstr (l = r)
    | Eq(Lam(_, l), Lam(_, r)) -> ctx_add_cstr_eq_expr l r
    | Eq(Lit(l), Lit(r)) -> ctx_ensure cstr (l = r)
    | Eq(LitTy, LitTy) -> return ()
    | Eq(Local(l), Local(r)) -> ctx_ensure cstr (l = r)
    | Eq(Pi(_, lt, lb), Pi(_, rt, rb)) -> ctx_add_cstr_eq_expr lt rt >> ctx_add_cstr_eq_expr lb rb
    | Eq(Universe(l), Universe(r)) -> ctx_ensure cstr (l = r)
    (* Type relation *)
    | Ty(Global(l), r) ->
        begin
          let+ ctx = get in
          match List.assoc_opt l ctx.defs with
          | Some(l) -> ctx_add_cstr_eq (Uast.from_tast l.value) r
          | None -> raise (Check_names.Unbound_name(l))
        end
    | Ty(LitTy, r) -> ctx_add_cstr_eq (Universe(0)) r
    | Ty(Universe(l), r) -> ctx_add_cstr_eq (Universe(l+1)) r
    | cstr -> raise (Failed_to_solve(cstr))
  in
  let rec solve_all () : (ctx, unit) State.t =
    let+ ctx = get in
    match ctx.cstrs with
    | [] -> return ()
    | hd::tl ->
        modify (fun ctx -> { ctx with cstrs = tl }) >>
        solve_one hd >>
        solve_all ()
  in
  solve_all () >>
  let+ ctx = get in
  let expr = walk ctx.subst 0
  and ty = walk ctx.subst 1 in
  modify (fun ctx -> { ctx with fresh = 2; subst = [] }) >>
  return { Tast.type_ = ty; Tast.value = expr }

let fresh (cb: Uast.expr_inner -> (ctx, 'a) State.t) : (ctx, 'a) State.t =
  let+ ctx = get in
  modify (fun ctx -> { ctx with fresh = ctx.fresh + 1 }) >>
  cb (Uast.Logic(ctx.fresh))

let from_ast (defs: Ast.def list) : int -> Ast.expr -> (ctx, Uast.expr_inner) State.t =
  let rec to_expr_inner (given: int) : Ast.expr -> (ctx, Uast.expr_inner) State.t = function
  | App(f, x) ->
      let+ f = to_expr 0 f
      and+ x = to_expr 0 x
      in return (Uast.App(f, x))
  | AppI(f, x) ->
      let+ f = to_expr (given+1) f
      and+ x = to_expr 0 x
      in return (Uast.App(f, x))
  | Global(s) -> let _ = (defs, given) in return (Uast.Global(s)) (* TODO *)
  | Hole -> fresh return
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
  and to_expr (given: int) (expr: Ast.expr) : (ctx, Uast.expr) State.t =
    let+ expr = to_expr_inner given expr
    in fresh (fun t -> return { Uast.type_ = t; Uast.value = expr })
  in to_expr_inner

let tyck_expr (expr: Ast.expr) (ty: Tast.expr_inner) : (ctx, Tast.expr) State.t =
  let+ ctx = State.get in
  let ty = Uast.from_tast ty in
  let+ expr = from_ast ctx.ast_defs 0 expr in
  modify (fun ctx -> { ctx with subst = [(0, expr); (1, ty)] }) >>
  ctx_add_cstr_ty (Logic(0)) (Logic(1)) >>
  ctx_finish

let add_ctor (dt: Ast.defty) (c: Ast.ctor) : (ctx, unit) State.t =
  let _ = (dt, c) in
  return "TODO add_ctor" >>= failwith

let add_defty (dt: Ast.defty) : (ctx, unit) State.t =
  assert (dt.pargs = []);
  assert (dt.iargs = []);
  ctx_add_def dt.name { type_ = Tast.Universe(1); value = Tast.Universe(0) }

let add_elim (dt: Ast.defty) : (ctx, unit) State.t =
  let _ = dt in
  return "TODO add_elim" >>= failwith

let tyck_def (def: Ast.def) : (ctx, unit) State.t =
  return ("tyck_def " ^ Ast.string_of_def def) >>= (return %% prerr_endline) >>= fun _ ->
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
  end >>= fun _ -> return (prerr_endline "===========================")

let tyck_module' (m: Ast.module_) : (ctx, unit) State.t =
  forM_ m.defs (fun def -> tyck_def def >> ctx_add_ast_def def)

let tyck_module (m: Ast.module_) : Tast.module_ =
  let ctx = exec ctx_empty (tyck_module' m) in
  { name = m.name; defs = ctx.defs }
