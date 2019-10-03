open Utils

type t
  = App of t * t
  | Lam of t
  | LitName of string
  | LitNum of int
  | Var of int

let rec to_string_prec : t -> int -> string = function
  | App(f, x) -> opt_parens (to_string_prec f 1 ^ " " ^ to_string_prec x 0) 1
  | Lam(b) -> opt_parens ("λ" ^ to_string_prec b 0) 1
  | LitName(n) -> const ("'" ^ n)
  | LitNum(n) -> const ("'" ^ string_of_int n)
  | Var(n) -> const (string_of_int n)

let to_string (expr: t) : string =
  to_string_prec expr 1

let rec compile_to_cam : t -> Cam.t = function
  | App(f, x) -> Cam.Com(Cam.App, Cam.Pair(compile_to_cam f, compile_to_cam x))
  | Lam(b) -> Cam.Lam(compile_to_cam b)
  | LitName(n) -> Cam.QuoteName(n)
  | LitNum(n) -> Cam.QuoteNum(n)
  | Var(0) -> Cam.Snd
  | Var(n) -> Cam.Com(compile_to_cam (Var(n-1)), Cam.Fst)

let compile_to_forth : t -> Forth.program =
  Cam.compile_to_forth %% Cam.optimize %% compile_to_cam
