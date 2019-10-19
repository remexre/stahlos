type 'a set = ('a, unit) Hashtbl.t

val (%%) : ('b -> 'c) -> ('a -> 'b) -> ('a -> 'c)

val const : 'a -> 'b -> 'a

val flip : ('a -> 'b -> 'c) -> ('b -> 'a -> 'c)

val genint : unit -> int

val gensym : unit -> string

val id : 'a -> 'a

val init : 'a list -> 'a list

val insert_and_get : 'a set -> 'a -> bool

val join_with : string -> string list -> string

val last : 'a list -> 'a

val logf : ('a, out_channel, unit) format -> 'a

val logln : string -> unit

val log_set_enabled : bool -> unit

val map_string : (char -> 'a) -> string -> 'a list

val must : ('b -> string) -> ('a, 'b) result -> 'a

val opt_parens : string -> int -> int -> string

val read_all_string : in_channel -> string

val read_file_string : string -> string

val with_output : string option -> (out_channel -> unit) -> unit
