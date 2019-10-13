open Utils

type expr =
  { type_ : expr_inner
  ; value : expr_inner
  }

and expr_inner
  = App of expr * expr
  | Global of string
  | Lam of string * expr
  | Lit of Sexpr.t
  | LitTy
  | Local of int
  | Pi of string * expr * expr
  | Universe of int

let rec string_of_expr (expr: expr) : string =
  string_of_expr_inner expr.value

and string_of_expr_inner : expr_inner -> string = function
  | App(f, x) -> let _ = (f, x) in failwith "TODO Tast.string_of_expr_inner App"
  | Global(s) -> s
  | Lam(n, b) -> let _ = (n, b) in failwith "TODO Tast.string_of_expr_inner Lam"
  | Lit(l) -> let _ = l in failwith "TODO Tast.string_of_expr_inner Lit"
  | LitTy -> "Erased%Type"
  | Local(n) -> "$" ^ string_of_int n
  | Pi(n, t, b) -> let _ = (n, t, b) in failwith "TODO Tast.string_of_expr_inner Pi"
  | Universe(n) -> "Type%" ^ string_of_int n

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
