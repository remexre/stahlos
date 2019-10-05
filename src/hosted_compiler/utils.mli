val (%%) : ('b -> 'c) -> ('a -> 'b) -> ('a -> 'c)

val const : 'a -> 'b -> 'a

val gensym : unit -> string

val id : 'a -> 'a

val join_with : string -> string list -> string

val map_string : (char -> 'a) -> string -> 'a list

val must : ('b -> string) -> ('a, 'b) result -> 'a

val opt_parens : string -> int -> int -> string

val read_all_string : in_channel -> string

val read_file_string : string -> string
