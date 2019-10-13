open Utils

module type State = sig
  type ('s, 'a) t

  val return : 'a -> ('s, 'a) t
  val (>>=) : ('s, 'a) t -> ('a -> ('s, 'b) t) -> ('s, 'b) t
  val (>>) : ('s, 'a) t -> ('s, 'b) t -> ('s, 'b) t

  val (let+) : ('s, 'a) t -> ('a -> ('s, 'b) t) -> ('s, 'b) t
  val (and+) : ('s, 'a) t -> ('s, 'b) t -> ('s, 'a * 'b) t

  val run : 's -> ('s, 'a) t -> 'a * 's
  val eval : 's -> ('s, 'a) t -> 'a
  val exec : 's -> ('s, unit) t -> 's

  val get : ('s, 's) t
  val put : 's -> ('s, unit) t
  val modify : ('s -> 's) -> ('s, unit) t

  val mapM : ('a -> ('s, 'b) t) -> 'a list -> ('s, 'b list) t
  val mapM_ : ('a -> ('s, unit) t) -> 'a list -> ('s, unit) t

  val forM : 'a list -> ('a -> ('s, 'b) t) -> ('s, 'b list) t
  val forM_ : 'a list -> ('a -> ('s, unit) t) -> ('s, unit) t
end

module State_pure : State= struct
  type ('s, 'a) t = { run : 's -> 'a * 's }

  let return (x: 'a) : ('s, 'a) t =
    { run = fun s -> (x, s) }

  let (>>=) (x: ('s, 'a) t) (f: 'a -> ('s, 'b) t) : ('s, 'b) t =
    { run = fun s -> let (x', s') = x.run s in (f x').run s' }

  let (>>) (x: ('s, 'a) t) (y: ('s, 'b) t) : ('s, 'b) t =
    x >>= const y

  let (let+) (x: ('s, 'a) t) (f: 'a -> ('s, 'b) t) : ('s, 'b) t =
    x >>= f

  let (and+) (x: ('s, 'a) t) (y: ('s, 'b) t) : ('s, 'a * 'b) t =
    let+ x' = x in
    let+ y' = y in
    return (x', y')

  let run (s: 's) (m: ('s, 'a) t) : 'a * 's =
    m.run s

  let eval (s: 's) (m: ('s, 'a) t) : 'a =
    fst (run s m)

  let exec (s: 's) (m: ('s, unit) t) : 's =
    snd (run s m)

  let get : ('s, 's) t =
    { run = fun s -> (s, s) }

  let put (s: 's) : ('s, unit) t =
    { run = fun _ -> ((), s) }

  let modify (f: 's -> 's) : ('s, unit) t =
    get >>= put %% f

  let rec mapM (f: 'a -> ('s, 'b) t) : 'a list -> ('s, 'b list) t = function
    | [] -> return []
    | hd::tl ->
        let+ hd' = f hd
        and+ tl' = mapM f tl
        in return (hd'::tl')

  let rec mapM_ (f: 'a -> ('s, unit) t) : 'a list -> ('s, unit) t = function
    | [] -> return ()
    | hd::tl -> f hd >> mapM_ f tl

  let forM (l: 'a list) (f: 'a -> ('s, 'b) t) : ('s, 'b list) t = mapM f l
  let forM_ (l: 'a list) (f: 'a -> ('s, unit) t) : ('s, unit) t = mapM_ f l
end
