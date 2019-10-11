type 'a t = { run : 'a * bool }

let return (x: 'a) : 'a t =
  { run = (x, false) }
let (let+) (x: 'a t) (f: 'a -> 'b t) : 'b t =
  let (x', flag1) = x.run in
  let (y, flag2) = (f x').run in
  { run = (y, flag1 || flag2) }
let (and+) (x: 'a t) (y: 'b t) : ('a * 'b) t =
  let+ x' = x in
  let+ y' = y in
  return (x', y')
let (>>) (x: 'a t) (y: 'b t) : 'b t =
  let+ _ = x in
  y

let set : unit t =
  { run = ((), true) }
