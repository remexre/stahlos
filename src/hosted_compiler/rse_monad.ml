type ('r, 's, 'e, 'a) t = { run : 'r -> 's -> (('a, 'e) result * 's) }

let return (x: 'a) : ('r, 's, 'e, 'a) t =
  { run = fun _ s -> (Ok(x), s) }

let (>>=) (m: ('r, 's, 'e, 'a) t) (f: 'a -> ('r, 's, 'e, 'b) t) : ('r, 's, 'e, 'b) t =
  { run = fun r s ->
            let (x, s') = m.run r s
            in match x with
            | Ok(x') -> (f x').run r s'
            | Error(e) -> (Error(e), s') }

let read : ('r, 's, 'e, 'r) t =
  { run = fun r s -> (Ok(r), s) }

let get : ('r, 's, 'e, 's) t =
  { run = fun _ s -> (Ok(s), s) }

let modify (f: 's -> 's) : ('r, 's, 'e, unit) t =
  { run = fun _ s -> (Ok(), f s) }

let throw (err: 'e) : ('r, 's, 'e, 'a) t =
  { run = fun _ s -> (Error(err), s) }

let catch (m: ('r, 's, 'e, 'a) t) : ('r, 's, 'e, ('a, 'e) result) t =
  { run = fun r s ->
            let (x, s') = m.run r s in
            (Ok(x), s') }

let (<|>) (x: ('r, 's, 'e, 'a) t) (y: ('r, 's, 'e, 'a) t) : ('r, 's, 'e, 'a) t =
  { run = fun r s ->
            let (x', s') = x.run r s
            in match x' with
            | Ok(_) -> (x', s')
            | Error(_) -> y.run r s }
