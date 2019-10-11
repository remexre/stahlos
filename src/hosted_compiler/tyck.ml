open State_monad

exception Type_error of expr * expr

type ctx_entry =
  { type_ : Tast.expr
  ; value : Tast.expr option
  }
type ctx = (string * ctx_entry) list

(* type subst = (int * expr) list *)

let tyck_def : Ast.def -> (ctx, Tast.def) State_monad.t = function
  | _ -> failwith "TODO"

let tyck_module' (m: Ast.module_) : (ctx, Tast.module_) State_monad.t =
  let+ defs' = forM m.defs tyck_def in
  let _ = defs' in
  failwith "TODO"

let tyck_module (m: Ast.module_) : Tast.module_ =
  fst ((tyck_module' m).run [])
