let (%%) (g: 'b -> 'c) (f: 'a -> 'b) (x: 'a) : 'c = g (f x)

let const (x: 'a) (_: 'b) : 'a = x

let gensym : unit -> string =
  let counter = ref 0 in
  fun () -> begin
    let n = !counter in
    counter := (n + 1);
    "gensym@" ^ string_of_int n
  end

let id (x: 'a) : 'a = x

let join_with (sep: string) : string list -> string =
  let rec helper : string list -> string = function
  | [] -> ""
  | h::t -> sep ^ h ^ helper t
  in function
  | [] -> ""
  | h::t -> h ^ helper t

let map_string (f: char -> 'a) (s: string) : 'a list =
  let rec helper (i: int) (acc : 'a list) : 'a list =
    if i = 0 then
      acc
    else
      helper (i-1) (f (String.get s (i-1)) :: acc)
  in helper (String.length s) []

let must (to_string: 'b -> string) : ('a, 'b) result -> 'a = function
  | Ok(x) -> x
  | Error(e) -> failwith (to_string e)

let opt_parens (s: string) (x: int) (y: int) : string =
  if x > y then "(" ^ s ^ ")" else s
