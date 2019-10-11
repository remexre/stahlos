open Utils

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
