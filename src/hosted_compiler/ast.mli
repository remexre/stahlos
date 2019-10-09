exception Invalid_ast of string * Sexpr.t

type expr
  = App of expr * expr
  | Global of string
  | Hole
  | Lam of string * expr
  | Lit of Sexpr.t
  | Local of int
  | Pi of string * expr * expr
  | Universe

val expr_of_sexpr : string list -> Sexpr.t -> expr
val sexpr_of_expr : expr -> Sexpr.t
val string_of_expr : expr -> string

type defty =
  { name : string
  ; iargs : (string * expr) list
  ; pargs : expr list
  }

type ctor =
  { name : string
  ; args_tys : expr list
  ; tyarg_tys : expr list
  }

type def
  = Def of string * expr * expr
  | Deftype of defty * ctor list

val def_of_sexpr : Sexpr.t -> def
val sexpr_of_def : def -> Sexpr.t
val string_of_def : def -> string
