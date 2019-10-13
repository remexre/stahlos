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
  | Logic of int
  | Local of int
  | Pi of string * expr * expr
  | Universe of int

let rec string_of_expr (expr: expr) : string =
  string_of_expr_inner expr.value

and string_of_expr_inner : expr_inner -> string = function
  | App(f, x) -> let _ = (f, x) in failwith "TODO string_of_expr_inner App"
  | Global(s) -> s
  | Lam(n, b) -> let _ = (n, b) in failwith "TODO string_of_expr_inner Lam"
  | Lit(l) -> let _ = l in failwith "TODO string_of_expr_inner Lit"
  | LitTy -> "Erased%Type"
  | Local(n) -> "$" ^ string_of_int n
  | Logic(n) -> "?%" ^ string_of_int n
  | Pi(n, t, b) -> let _ = (n, t, b) in failwith "TODO string_of_expr_inner Pi"
  | Universe(n) -> "Type%" ^ string_of_int n

let from_tast : Tast.expr_inner -> expr_inner = function
  | App(f, x) -> let _ = (f, x) in failwith "TODO from_tast_inner App"
  | Global(s) -> let _ = s in failwith "TODO from_tast_inner Global"
  | Lam(n, b) -> let _ = (n, b) in failwith "TODO from_tast_inner Lam"
  | Lit(l) -> let _ = l in failwith "TODO from_tast_inner Lit"
  | LitTy -> LitTy
  | Local(n) -> let _ = n in failwith "TODO from_tast_inner Local"
  | Pi(n, t, b) -> let _ = (n, t, b) in failwith "TODO from_tast_inner Pi"
  | Universe(n) -> Universe(n)
