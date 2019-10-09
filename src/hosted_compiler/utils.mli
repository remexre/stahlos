val (%%) : ('b -> 'c) -> ('a -> 'b) -> ('a -> 'c)

val const : 'a -> 'b -> 'a

val flip : ('a -> 'b -> 'c) -> ('b -> 'a -> 'c)

val gensym : unit -> string

val id : 'a -> 'a

val init : 'a list -> 'a list

val join_with : string -> string list -> string

val last : 'a list -> 'a

val map_string : (char -> 'a) -> string -> 'a list

val must : ('b -> string) -> ('a, 'b) result -> 'a

val opt_parens : string -> int -> int -> string

val read_all_string : in_channel -> string

val read_file_string : string -> string
