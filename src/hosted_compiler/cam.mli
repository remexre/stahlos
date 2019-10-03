type t
  = App
  | Fst
  | Snd
  | Id
  | Add
  | Mul
  | QuoteName of string
  | QuoteNum of int
  | Lam of t
  | Com of t * t
  | Pair of t * t

val to_string : t -> string

val optimize : t -> t

val compile_to_forth : t -> Forth.program
