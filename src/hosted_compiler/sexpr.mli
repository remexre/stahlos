type t
  = Atom of string
  | Int of Int64.t
  | List of t list
  | String of string

val to_string : t -> string

val parse : string -> (t list, string) result
