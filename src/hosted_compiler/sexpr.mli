type t
  = Int of Int64.t
  | List of t list
  | Symbol of string

val to_string : t -> string

val parse : string -> (t list, string) result
