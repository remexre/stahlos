type 'a t = { run : 'a * bool }

val return : 'a -> 'a t
val (>>=) : 'a t -> ('a -> 'b t) -> 'b t
val (>>) : 'a t -> 'b t -> 'b t

val set : unit t
