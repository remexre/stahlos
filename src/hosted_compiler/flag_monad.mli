type 'a t = { run : 'a * bool }

val return : 'a -> 'a t
val (let+) : 'a t -> ('a -> 'b t) -> 'b t
val (and+) : 'a t -> 'b t -> ('a * 'b) t
val (>>) : 'a t -> 'b t -> 'b t

val set : unit t
