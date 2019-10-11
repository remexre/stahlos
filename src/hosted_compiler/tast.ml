open Utils

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

let sexpr_of_expr_inner : expr_inner -> Sexpr.t = function
  | App(f, x) -> let _ = (f, x) in failwith "TODO"
  | Global(s) -> let _ = s in failwith "TODO"
  | Lam(n, b) -> let _ = (n, b) in failwith "TODO"
  | Lit(f, l) -> let _ = (f, l) in failwith "TODO"
  | Local(n) -> let _ = n in failwith "TODO"
  | Pi(n, t, b) -> let _ = (n, t, b) in failwith "TODO"
  | Universe(n) -> let _ = n in failwith "TODO"

let sexpr_of_expr (expr: expr) : Sexpr.t =
  sexpr_of_expr_inner expr.value

let string_of_expr : expr -> string =
  Sexpr.to_string %% sexpr_of_expr

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

let sexpr_of_def : def -> Sexpr.t = function
  | Def(n, t, e) -> List([Atom("def"); Atom(n); sexpr_of_expr t; sexpr_of_expr e])
  | Deftype(dt, cs) -> let _ = (dt, cs) in failwith "TODO"

let string_of_def : def -> string =
  Sexpr.to_string %% sexpr_of_def

type module_ =
  { name : string
  ; defs : def list
  }

let sexprs_of_module (m: module_) : Sexpr.t list =
  let _ = m in
  failwith "TODO"

let string_of_module : module_ -> string =
  join_with "\n" %% List.map Sexpr.to_string %% sexprs_of_module
