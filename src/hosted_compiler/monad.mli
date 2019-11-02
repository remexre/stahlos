module type Type = sig type t end

module type Monad_minimal = sig
  type 'a t

  val return : 'a -> 'a t
  val (>>=) : 'a t -> ('a -> 'b t) -> 'b t
end

module type Monad = sig
  include Monad_minimal

  val map : ('a -> 'b) -> 'a t -> 'b t
  val (>>) : 'a t -> 'b t -> 'b t

  val (let+) : 'a t -> ('a -> 'b t) -> 'b t
  val (and+) : 'a t -> 'b t -> ('a * 'b) t

  val mapM : ('a -> 'b t) -> 'a list -> 'b list t
  val mapM_ : ('a -> unit t) -> 'a list -> unit t

  val forM : 'a list -> ('a -> 'b t) -> 'b list t
  val forM_ : 'a list -> ('a -> unit t) -> unit t
end

module Extend : functor (M: Monad_minimal) -> Monad

module type Semigroup = sig
  include Type

  val op : t -> t -> t
end

module type Monoid = sig
  include Semigroup

  val id : t
end

module type Fresh = sig
  include Monad

  type f

  val fresh : f t
end

module type Reader = sig
  include Monad

  type r

  val ask : r t
end

module type State = sig
  include Monad

  type s

  val get : s t
  val put : s -> unit t
  val modify : (s -> s) -> unit t
end

module type Writer = sig
  include Monad

  type w

  val tell : w -> unit t
end

module type RWS = sig
  include Monad
  include Reader with type 'a t := 'a t
  include State with type 'a t := 'a t
  include Writer with type 'a t := 'a t
end

module type Lift = sig
  include Monad

  type 'a m

  val lift : 'a m -> 'a t
end

module Monoid_bool_or : Monoid with type t = bool

module Monoid_list_append : functor (T : Type) -> Monoid with type t = T.t list

module Monoid_unit : Monoid with type t = unit

module Identity : Monad with type 'a t = 'a

module Reader_t : functor (T: Type) (M: Monad) -> sig
  include Reader with type r := T.t
  include Lift with type 'a m := 'a M.t and type 'a t := 'a t

  val run : T.t -> 'a t -> 'a M.t
end

module State_t : functor (T: Type) (M: Monad) -> sig
  include State with type s := T.t
  include Lift with type 'a m := 'a M.t and type 'a t := 'a t

  val run : T.t -> 'a t -> ('a * T.t) M.t
  val eval : T.t -> 'a t -> 'a M.t
  val exec : T.t -> unit t -> T.t M.t
end

module Writer_t : functor (T: Monoid) (M: Monad) -> sig
  include Writer with type w := T.t
  include Lift with type 'a m := 'a M.t and type 'a t := 'a t

  val run : 'a t -> ('a * T.t) M.t
  val eval : 'a t -> 'a M.t
  val exec : unit t -> T.t M.t
end

module RWS_t : functor (R: Type) (W: Monoid) (S: Type) (M: Monad) -> sig
  include RWS with type r := R.t and type w := W.t and type s := S.t
  include Lift with type 'a m := 'a M.t and type 'a t := 'a t

  val run : 'a t -> R.t -> S.t -> ('a * S.t * W.t) M.t
end
