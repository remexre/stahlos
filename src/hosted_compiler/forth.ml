open Utils

type word
  = App
  | Lam of string
  | Fst
  | Snd
  | Cons
  | Push
  | Swap
  | QuoteName of string
  | QuoteNum of int
  | Add
  | Mul

type program =
  { defs : (string * word list) list
  ; main : word list
  }

let rec optimize : word list -> word list = function
  | [] -> []
  | Push::Swap::tl -> Push::optimize tl
  | hd::tl -> hd::optimize tl

let string_of_word : word -> string = function
  | App -> "app"
  | Lam(b) -> "' " ^ b ^ " lam"
  | Fst -> "fst"
  | Snd -> "snd"
  | Cons -> "cons"
  | Push -> "push"
  | Swap -> "swap"
  | QuoteName(n) -> "drop ' " ^ n
  | QuoteNum(n) -> "drop " ^ string_of_int n
  | Add -> "add"
  | Mul -> "mul"

let string_of_program (program: program) : string =
  let string_of_words : word list -> string = join_with " " %% List.map string_of_word in
  let helper ((name, words): string * word list) : string =
    ": " ^ name ^ " " ^ string_of_words words ^ " ;"
  in join_with "\n" (List.map helper program.defs) ^ "\n" ^ string_of_words program.main
