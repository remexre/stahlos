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

let string_of_expr_inner : expr_inner -> string = function
  | App(f, x) -> let _ = (f, x) in failwith "TODO"
  | Global(s) -> let _ = s in failwith "TODO"
  | Lam(n, b) -> let _ = (n, b) in failwith "TODO"
  | Lit(f, l) -> let _ = (f, l) in failwith "TODO"
  | Local(n) -> let _ = n in failwith "TODO"
  | Pi(n, t, b) -> let _ = (n, t, b) in failwith "TODO"
  | Universe(n) -> let _ = n in failwith "TODO"

let string_of_expr (expr: expr) : string =
  let _ = expr in
  failwith "TODO"

type module_ =
  { name : string
  ; defs : (string * expr) list
  }

let string_of_def (name, expr) =
  Printf.sprintf "(def %s %s %s)" name
    (string_of_expr_inner expr.type_)
    (string_of_expr_inner expr.value)

let string_of_module (m: module_) : string =
  join_with "\n" ((Printf.sprintf "(module %s)" m.name)::(List.map string_of_def m.defs))
