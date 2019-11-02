open Tyck_solve
open Uast
open Utils

let push cs c = cs := (c::!cs)

let rec collect_constraints cs expr =
  (* TODO: Normalize *)
  collect_constraints_inner cs expr.type_;
  collect_constraints_inner cs expr.value;
  push cs (Ty(expr.value, expr.type_))
and collect_constraints_inner cs e =
  match e with
  | Global(_) -> ()
  | Lam(_, b) -> collect_constraints cs b
  | LitTy(_) -> push cs (Ty(e, Universe(0)))
  | Local(_) -> ()
  | Pi(_, t, b) ->
      collect_constraints cs t;
      collect_constraints cs b;
      let t' = VarU("", genint ()) in
      push cs (Lub(t', b.type_, t.type_))
  | Universe(_) -> ()
  | VarE(_, _) -> ()
  | VarU(_, _) -> ()
  | e -> failwith ("TODO collect_constraints_inner: " ^ Uast.string_of_expr_inner e)

let tyck_expr ctx expr =
  let cs =
    let cs = ref [] in
    collect_constraints cs expr;
    !cs
  in
  Uast.tast_of_expr (solve ctx cs) expr

let rec make_pi args ret =
  match args with
  | [] -> ret
  | (n, t)::tl -> Pi(n, { type_ = VarU(n, genint())
                        ; value = from_nast t
                        }
                      , { type_ = VarU("", genint ())
                        ; value = make_pi tl ret
                        })

let tyck_defty ctx (dt: Nast.defty) =
  [ (dt.name, tyck_expr ctx { type_ = make_pi dt.args (Universe(0))
                            ; value = LitTy(dt.name)
                            })
  (* ; ("elim-" ^ dt.name, TODO) *)
  ]

let tyck_def ctx def = match def with
  | Nast.Def(n, _, t, e) ->
      [(n, tyck_expr ctx { type_ = from_nast t; value = from_nast e })]
  | Nast.Deftype(dt, cs) ->
      assert (dt.args = []);
      assert (cs = []);
      tyck_defty ctx dt

let tast_type e = let open Tast in e.type_
let tast_value e = let open Tast in e.value

let rec tyck_defs ctx = function
  | [] -> []
  | hd::tl -> let defs = tyck_def ctx hd in
              let module S = Show.List(Show.Pair(Show.String)(Tast.Show.Expr)) in
              logln (S.show defs);
              defs @ tyck_defs (List.map (second tast_type) defs @ ctx) tl

let tyck_module m =
  let open Nast in
  { Tast.name = m.name
  ; Tast.defs = tyck_defs (List.map (second tast_value) Builtins.builtins) m.defs
  }
