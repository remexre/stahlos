type 'a t = { run : 'a * bool }

let return (x: 'a) : 'a t =
  { run = (x, false) }
let (>>=) (x: 'a t) (f: 'a -> 'b t) : 'b t =
  let (x', flag1) = x.run in
  let (y, flag2) = (f x').run in
  { run = (y, flag1 || flag2) }
let (>>) (x: 'a t) (y: 'b t) : 'b t =
  x >>= fun _ -> y

let set : unit t =
  { run = ((), true) }
