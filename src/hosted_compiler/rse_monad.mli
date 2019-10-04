type ('r, 's, 'e, 'a) t = { run : 'r -> 's -> (('a, 'e) result * 's) }

val return : 'a -> ('r, 's, 'e, 'a) t
val (>>=) : ('r, 's, 'e, 'a) t -> ('a -> ('r, 's, 'e, 'b) t) -> ('r, 's, 'e, 'b) t

val read : ('r, 's, 'e, 'r) t

val get : ('r, 's, 'e, 's) t
val modify : ('s -> 's) -> ('r, 's, 'e, unit) t

val throw : 'e -> ('r, 's, 'e, 'a) t
val catch : ('r, 's, 'e, 'a) t -> ('r, 's, 'e, ('a, 'e) result) t
val (<|>) : ('r, 's, 'e, 'a) t -> ('r, 's, 'e, 'a) t -> ('r, 's, 'e, 'a) t
