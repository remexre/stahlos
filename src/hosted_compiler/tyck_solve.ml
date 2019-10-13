open Tyck_ctx
open Utils

let walk subst =
  let rec lookup n =
    match List.assoc_opt n subst with
    | Some(e) -> walk_expr_inner e
    | None -> raise (Tyck_ctx.Unsolved_variable(n))
  and walk_expr (expr: Uast.expr) : Tast.expr =
    { type_ = walk_expr_inner expr.type_; value = walk_expr_inner expr.value }
  and walk_expr_inner expr =
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

let solve_one cstr =
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
        | None -> raise (Tyck_ctx.Failed_to_solve(cstr))
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
  | cstr -> raise (Tyck_ctx.Failed_to_solve(cstr))

let rec solve_all () =
  let+ ctx = get in
  match ctx.cstrs with
  | [] -> return ()
  | hd::tl ->
      modify (fun ctx -> { ctx with cstrs = tl }) >>
      solve_one hd >>
      solve_all ()

let ctx_finish =
  solve_all () >>
  let+ ctx = get in
  let expr = walk ctx.subst 0
  and ty = walk ctx.subst 1 in
  modify (fun ctx -> { ctx with fresh = 2; subst = [] }) >>
  return { Tast.type_ = ty; Tast.value = expr }
