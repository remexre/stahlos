exception Type_error of expr * expr

type ctx_entry =
  { type_ : Tast.expr
  ; value : Tast.expr option
  }
type ctx = (string * ctx_entry) list

val tyck_module : Ast.module_ -> Tast.module_
