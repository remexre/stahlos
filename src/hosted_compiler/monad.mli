module type Type = sig type t end

module type Monad_minimal = sig
  type 'a t

  val return : 'a -> 'a t
  val (>>=) : 'a t -> ('a -> 'b t) -> 'b t
end

module type Monad = sig
  include Monad_minimal

  val (>>) : 'a t -> 'b t -> 'b t

  val (let+) : 'a t -> ('a -> 'b t) -> 'b t
  val (and+) : 'a t -> 'b t -> ('a * 'b) t

  val mapM : ('a -> 'b t) -> 'a list -> 'b list t
  val mapM_ : ('a -> unit t) -> 'a list -> unit t

  val forM : 'a list -> ('a -> 'b t) -> 'b list t
  val forM_ : 'a list -> ('a -> unit t) -> unit t
end

module Extend : functor (M: Monad_minimal) -> Monad

module type Fresh = sig
  include Monad

  type f

  val fresh : f t
end

module type State = sig
  include Monad

  type s

  val get : s t
  val put : s -> unit t
  val modify : (s -> s) -> unit t
end

module State_pure : functor (T: Type) -> sig
  include State with type s := T.t

  val run : T.t -> 'a t -> 'a * T.t
  val eval : T.t -> 'a t -> 'a
  val exec : T.t -> unit t -> T.t
end
