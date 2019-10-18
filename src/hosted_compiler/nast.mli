type expr
  = App of expr * expr
  | Global of string
  | Hole
  | Lam of string * expr
  | Lit of Sexpr.t
  | Local of int
  | Pi of string * expr * expr
  | Universe

type defty =
  { name : string
  ; implicits : int
  ; args : (string * expr) list
  }

type ctor =
  { name : string
  ; args : (string * expr) list
  ; tyargs : expr list
  }

type def
  = Def of string * int * expr * expr
  | Deftype of defty * ctor list

type module_ =
  { name : string
  ; defs : def list
  }

val resolve_names_for_module : Ast.module_ -> module_
val string_of_module : module_ -> string
