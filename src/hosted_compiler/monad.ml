open Utils

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

module Extend (M: Monad_minimal) = struct
  include M

  let map f x = M.(>>=) x (fun x' -> M.return (f x'))

  let (>>) x y =
    x >>= const y

  let (let+) x f =
    x >>= f

  let (and+) x y =
    let+ x' = x in
    let+ y' = y in
    return (x', y')

  let rec mapM f = function
    | [] -> return []
    | hd::tl ->
        let+ hd' = f hd
        and+ tl' = mapM f tl
        in return (hd'::tl')

  let rec mapM_ f = function
    | [] -> return ()
    | hd::tl -> f hd >> mapM_ f tl

  let forM l f = mapM f l
  let forM_ l f = mapM_ f l
end

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

module Monoid_bool_or = struct
  type t = bool
  let op = (||)
  let id = false
end

module Monoid_list_append (T : Type) = struct
  type t = T.t list
  let op = (@)
  let id = []
end

module Monoid_unit = struct
  type t = unit
  let op () () = ()
  let id = ()
end

module Identity = Extend(struct
  type 'a t = 'a

  let return x = x

  let (>>=) x f = f x
end)

module RWS_t (R: Type) (W: Monoid) (S: Type) (M: Monad) = struct
  type 'a m = { run : R.t -> S.t -> ('a * S.t * W.t) M.t }
  include Extend(struct
    type 'a t = 'a m

    let return x = { run = fun _ s -> M.return (x, s, W.id) }

    let (>>=) x f =
      { run = fun r s -> M.(>>=) (x.run r s)
             (fun (x', s', w) -> M.map
               (fun (y, s'', w') -> (y, s'', W.op w w'))
               ((f x').run r s')) }
  end)

  let ask = { run = fun r s -> M.return (r, s, W.id) }

  let tell w = { run = fun _ s -> M.return ((), s, w) }

  let get = { run = fun _ s -> M.return (s, s, W.id) }
  let put s = { run = fun _ _ -> M.return ((), s, W.id) }
  let modify f = { run = fun _ s -> M.return ((), f s, W.id) }

  let lift x = { run = fun _ s -> M.map (fun x' -> (x', s, W.id)) x }

  let run x r s = x.run r s
end

module Reader_t (T: Type) (M: Monad) = struct
  include RWS_t(T)(Monoid_unit)(Monoid_unit)(M)

  let run r x = M.map (fun (x, _, _) -> x) (x.run r ())
end

module State_t (T: Type) (M: Monad) = struct
  include RWS_t(Monoid_unit)(Monoid_unit)(T)(M)

  let run s x = M.map (fun (x, s, ()) -> (x, s)) (x.run () s)
  let eval s x = M.map (fun (x, _, ()) -> x) (x.run () s)
  let exec s x = M.map (fun (_, s, ()) -> s) (x.run () s)
end

module Writer_t (T: Monoid) (M: Monad) = struct
  include RWS_t(Monoid_unit)(T)(Monoid_unit)(M)

  let run x = M.map (fun (x, (), w) -> (x, w)) (x.run () ())
  let eval x = M.map (fun (x, (), _) -> x) (x.run () ())
  let exec x = M.map (fun (_, (), w) -> w) (x.run () ())
end
