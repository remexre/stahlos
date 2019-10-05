type word
  = App
  | Lam of string
  | Fst
  | Snd
  | Cons
  | Dup
  | Swap
  | QuoteName of string
  | QuoteNum of int
  | Add
  | Mul

type program =
  { defs : (string * word list) list
  ; main : word list
  }

val optimize : word list -> word list

val string_of_word : word -> string

val string_of_program : program -> string
