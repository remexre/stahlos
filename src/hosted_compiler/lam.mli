type t
  = App of t * t
  | Lam of t
  | LitName of string
  | LitNum of int
  | Var of int

val to_string : t -> string

val compile_to_cam : t -> Cam.t

val compile_to_forth : t -> Forth.program
