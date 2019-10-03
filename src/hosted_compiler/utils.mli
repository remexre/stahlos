val (%%) : ('b -> 'c) -> ('a -> 'b) -> ('a -> 'c)

val const : 'a -> 'b -> 'a

val gensym : unit -> string

val join_with : string -> string list -> string

val must : ('b -> string) -> ('a, 'b) result -> 'a

val opt_parens : string -> int -> int -> string
