open Utils

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

module Extend (M: Monad_minimal) = struct
  include M

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

module Monoid_bool_or = struct
  type t = bool
  let op = (||)
  let id = false
end

module State_pure (T: Type) = struct
  type 'a m = { run : T.t -> 'a * T.t }
  include Extend(struct
    type 'a t = 'a m

    let return x = { run = fun s -> (x, s) }

    let (>>=) x f =
      { run = fun s -> let (x', s') = x.run s in (f x').run s' }
  end)

  let get = { run = fun s -> (s, s) }
  let put s = { run = fun _ -> ((), s) }
  let modify f = get >>= put %% f

  let run s x = x.run s
  let eval s x = fst (run s x)
  let exec s x = snd (run s x)
end

module Writer_pure (M: Monoid) = struct
  type 'a m = { run : 'a * M.t }
  include Extend(struct
    type 'a t = 'a m

    let return x = { run = (x, M.id) }

    let (>>=) x f =
      let (x', w) = x.run in
      let (y', w') = (f x').run in
      { run = (y', M.op w w') }
  end)

  let tell w = { run = ((), w) }

  let run x = x.run
  let eval x = fst (x.run)
  let exec x = snd (x.run)
end
