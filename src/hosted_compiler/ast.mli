exception Invalid_ast of string * Sexpr.t

type expr
  = App of expr * expr
  | AppI of expr * expr
  | Global of string
  | Hole of string
  | Lam of string * expr
  | LamI of string * expr
  | Lit of Sexpr.t
  | Local of int
  | Pi of string * expr * expr
  | PiI of string * expr * expr
  | Universe

val expr_of_sexpr : string list -> Sexpr.t -> expr
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

val def_of_sexpr : Sexpr.t -> def
val sexpr_of_def : def -> Sexpr.t
val string_of_def : def -> string

type module_ =
  { name : string
  ; defs : def list
  }

val module_of_sexprs : Sexpr.t list -> module_
val sexprs_of_module : module_ -> Sexpr.t list
val string_of_module : module_ -> string

val load_module_from : in_channel -> module_
val load_module_from_path : string -> module_
