type expr =
  { type_ : expr_inner
  ; value : expr_inner
  }

and expr_inner
  = App of expr * expr
  | Global of string
  | Lam of string * expr
  | Lit of string * Sexpr.t
  | Local of int
  | Pi of string * expr * expr
  | Universe of int

val sexpr_of_expr : expr -> Sexpr.t
val string_of_expr : expr -> string

type defty =
  { name : string
  ; pargs : (string * expr) list
  ; iargs : (string * expr) list
  }

type ctor =
  { name : string
  ; args : (string * expr) list
  ; tyargs : expr list
  }

type def
  = Def of string * expr * expr
  | Deftype of defty * ctor list

val sexpr_of_def : def -> Sexpr.t
val string_of_def : def -> string

type module_ =
  { name : string
  ; defs : def list
  }

val sexprs_of_module : module_ -> Sexpr.t list
val string_of_module : module_ -> string
