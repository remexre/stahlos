type ('s, 'a) t = { run : 's -> 'a * 's }

val return : 'a -> ('s, 'a) t
val (>>=) : ('s, 'a) t -> ('a -> ('s, 'b) t) -> ('s, 'b) t
val (>>) : ('s, 'a) t -> ('s, 'b) t -> ('s, 'b) t

val (let+) : ('s, 'a) t -> ('a -> ('s, 'b) t) -> ('s, 'b) t
val (and+) : ('s, 'a) t -> ('s, 'b) t -> ('s, 'a * 'b) t

val get : ('s, 's) t
val put : 's -> ('s, unit) t

val forM : 'a list -> ('a -> ('s, 'b) t) -> ('s, 'b list) t
