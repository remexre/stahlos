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
  | LitTy of string
  | Local of int
  | Pi of string * expr * expr
  | Universe
  | Var of string * int

type subst = (int * expr_inner) list

let rec walk subst expr =
  { type_ = walk_inner subst expr.type_
  ; value = walk_inner subst expr.value
  }
and walk_inner subst = function
  | App(f, x) -> App(walk subst f, walk subst x)
  | Global(s) -> Global(s)
  | Lam(n, b) -> Lam(n, walk subst b)
  | Lit(l) -> Lit(l)
  | LitTy(n) -> LitTy(n)
  | Local(n) -> Local(n)
  | Pi(n, t, b) -> Pi(n, walk subst t, walk subst b)
  | Universe -> Universe
  | Var(n, i) -> (match List.assoc_opt i subst with
                 | Some(e) -> walk_inner subst e
                 | _ -> Var(n, i))

let rec from_nast = function
  | Nast.App(f, x) -> App(expr_from_nast f, expr_from_nast x)
  | Nast.Global(s) -> Global(s)
  | Nast.Hole(n) -> Var(n, genint ())
  | Nast.Lam(n, b) -> Lam(n, expr_from_nast b)
  | Nast.Lit(l) -> Lit(l)
  | Nast.Local(n) -> Local(n)
  | Nast.Pi(n, t, b) -> Pi(n, expr_from_nast t, expr_from_nast b)
  | Nast.Universe -> Universe
and expr_from_nast nast =
  { type_ = Var("", genint ())
  ; value = from_nast nast
  }

let rec from_tast (e: Tast.expr) =
  { type_ = from_tast_inner e.type_
  ; value = from_tast_inner e.value
  }
and from_tast_inner = function
  | Tast.App(f, x) -> App(from_tast f, from_tast x)
  | Tast.Global(s) -> Global(s)
  | Tast.Lam(n, b) -> Lam(n, from_tast b)
  | Tast.Lit(l) -> Lit(l)
  | Tast.LitTy(n) -> LitTy(n)
  | Tast.Local(n) -> Local(n)
  | Tast.Pi(n, t, b) -> Pi(n, from_tast t, from_tast b)
  | Tast.Universe -> Universe

exception Unsolved_variable of string * int * expr list

let rec tast_of_expr subst expr =
  try
    { Tast.type_ = tast_of_expr_inner subst expr.type_
    ; Tast.value = tast_of_expr_inner subst expr.value
    }
  with
  | Unsolved_variable(n, i, es) -> raise (Unsolved_variable(n, i, expr::es))
and tast_of_expr_inner subst = function
  | App(f, x) -> Tast.App(tast_of_expr subst f, tast_of_expr subst x)
  | Global(s) -> Tast.Global(s)
  | Lam(n, b) -> Tast.Lam(n, tast_of_expr subst b)
  | Lit(l) -> Tast.Lit(l)
  | LitTy(n) -> Tast.LitTy(n)
  | Local(n) -> Tast.Local(n)
  | Pi(n, t, b) -> Tast.Pi(n, tast_of_expr subst t, tast_of_expr subst b)
  | Universe -> Tast.Universe
  | Var(n, i) -> (match List.assoc_opt i subst with
                 | Some(e) -> tast_of_expr_inner subst e
                 | _ -> raise (Unsolved_variable(n, i, [])))

let rec string_of_expr expr =
  string_of_expr_inner expr.value

and string_of_expr_inner = function
  | App(f, x) -> let _ = (f, x) in failwith "TODO Uast.string_of_expr_inner App"
  | Global(s) -> s
  | Lam(n, b) -> Printf.sprintf "(fn %s %s)" n (string_of_expr b)
  | Lit(l) -> let _ = l in failwith "TODO Uast.string_of_expr_inner Lit"
  | LitTy(n) -> "ErasedType%" ^ n
  | Local(n) -> "$" ^ string_of_int n
  | Pi(n, t, b) -> Printf.sprintf "(pi %s %s %s)" n (string_of_expr t) (string_of_expr b)
  | Universe -> "Type"
  | Var(n, i) -> Printf.sprintf "Var%%_%s.%d" n i
