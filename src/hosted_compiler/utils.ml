let (%%) (g: 'b -> 'c) (f: 'a -> 'b) (x: 'a) : 'c = g (f x)

let const (x: 'a) (_: 'b) : 'a = x

let flip (f: 'a -> 'b -> 'c) (y: 'b) (x: 'a) : 'c =
  f x y

let gensym : unit -> string =
  let counter = ref 0 in
  fun () -> begin
    let n = !counter in
    counter := (n + 1);
    "gensym@" ^ string_of_int n
  end

let id (x: 'a) : 'a = x

let rec init : 'a list -> 'a list = function
  | [] -> failwith "init"
  | [_] -> []
  | hd::tl -> hd :: init tl

let join_with (sep: string) : string list -> string =
  let rec helper : string list -> string = function
  | [] -> ""
  | h::t -> sep ^ h ^ helper t
  in function
  | [] -> ""
  | h::t -> h ^ helper t

let rec last : 'a list -> 'a = function
  | [] -> failwith "last"
  | [x] -> x
  | _::tl -> last tl

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

let read_all_string (chan: in_channel) : string =
  let chunks = ref [] in
  begin
    try
      let rec loop () =
        let buf = Bytes.create 1024 in
        let len = input chan buf 0 10 in
        if len > 0 then begin
          chunks := (Bytes.sub buf 0 len) :: !chunks;
          loop ()
        end
      in loop ()
    with
      End_of_file -> ()
  end;
  let chunks = !chunks in
  let total_len = List.fold_left (fun l b -> l + Bytes.length b) 0 chunks in
  let out = Bytes.create total_len in
  let blit l b =
      let l' = Bytes.length b in
      Bytes.blit b 0 out (total_len-l-l') l';
      l + l'
  in
  let _ = List.fold_left blit 0 chunks in
  Bytes.to_string out

let read_file_string (path: string) : string =
  let f = open_in path in
  try
    let s = read_all_string f in
    close_in f;
    s
  with
    e -> close_in f; raise e
