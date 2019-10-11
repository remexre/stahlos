open State_monad
open Uast

exception Type_error of expr * expr

type ctx =
  { ast_defs : Ast.def list
  ; defs : (string * Tast.expr) list
  ; fresh : int
  ; subst : (int * Uast.expr) list
  }

let ctx_empty : ctx = { ast_defs = []; defs = []; fresh = 0; subst = [] }

let ctx_finish (ast: Ast.def) : (ctx, unit) State_monad.t =
  modify (fun ctx -> { ast_defs = ast::ctx.ast_defs
                     ; defs = ctx.defs
                     ; fresh = 0
                     ; subst = [] })

let tyck_def : Ast.def -> (ctx, unit) State_monad.t = function
  | Ast.Def(n, t, e) -> let _ = (n, t, e) in return () (* failwith "TODO Def" *)
  | Ast.Deftype(dt, cs) -> let _ = (dt, cs) in return () (* failwith "TODO Deftype" *)

let tyck_module' (m: Ast.module_) : (ctx, unit) State_monad.t =
  forM_ m.defs (fun def -> tyck_def def >> ctx_finish def)

let tyck_module (m: Ast.module_) : Tast.module_ =
  let (_, ctx) = (tyck_module' m).run ctx_empty in
  { name = m.name; defs = ctx.defs }
