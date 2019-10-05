open Utils

type t
  = Int of Int64.t
  | List of t list
  | String of string
  | Symbol of string

let rec to_string : t -> string = function
  (* special cases *)
  | List([Symbol("quote"); e]) -> "'" ^ to_string e
  (* general cases *)
  | Int(x) -> Int64.to_string x
  | List(l) -> "(" ^ join_with " " (List.map to_string l) ^ ")"
  | String(s) -> "\"" ^ String.escaped s ^ "\""
  | Symbol(s) -> s



exception Parser_error of string

let advance (ctx: string * int ref) : unit =
  let r = snd ctx in
  r := (1 + !r)

let peek (ctx: string * int ref) : char option =
  let i = !(snd ctx)
  and l = String.length (fst ctx) in
  if i >= l then
    None
  else
    Some(String.get (fst ctx) i)

let expectation_failed (ctx: string * int ref) (wanted: string) : 'a =
  let i = !(snd ctx) in
  let msg = match peek ctx with
  | Some(c) -> Printf.sprintf "Expected %s, found '%s' at position %d" wanted (Char.escaped c) i
  | None -> Printf.sprintf "Expected %s, found EOF at position %d" wanted i
  in raise (Parser_error(msg))

let expect_eof (ctx: string * int ref) : unit =
  let i = !(snd ctx) in
  if i < String.length (fst ctx) then
    expectation_failed ctx "EOF"

let expect_char (ctx: string * int ref) (c: char) : unit =
  if peek ctx = Some(c) then
    advance ctx
  else
    expectation_failed ctx (Printf.sprintf "'%s'" (Char.escaped c))

let rec skip_whitespace (ctx: string * int ref) : unit =
  match peek ctx with
  | Some(c) when String.contains " \n" c -> advance ctx; skip_whitespace ctx
  | _ -> ()



let is_symbolish (ch: char) : bool =
  ('0' <= ch && ch <= '9') ||
  ('A' <= ch && ch <= 'Z') ||
  ('a' <= ch && ch <= 'z') ||
  String.contains "%*+-/<>?" ch

let parse_string (ctx: string * int ref) : string =
  let cs =
    let rec loop (cs: char list) : char list =
      let c = match peek ctx with
      | Some(c) -> c
      | None -> expectation_failed ctx "'\"'"
      in
      advance ctx;
      if c = '"' then
        cs
      else
        loop (c::cs)
    in loop []
  in
  let length = List.length cs in
  let buf = Bytes.create length in
  let _ =
    let helper (l: int) (c: char) : int =
      Bytes.set buf l c;
      l + 1
    in List.fold_left helper 0 cs
  in Bytes.to_string buf

let rec parse_symbolish (ctx: string * int ref) (start: int) : string =
  advance ctx;
  match peek ctx with
  | Some(c) when is_symbolish c -> parse_symbolish ctx start
  | _ -> String.sub (fst ctx) start (!(snd ctx) - start)

let rec parse_one (ctx: string * int ref) : t =
  match peek ctx with
  | Some('"') -> advance ctx;
                 String(parse_string ctx)
  | Some('\'') -> advance ctx;
                  skip_whitespace ctx;
                  let e = parse_one ctx in
                  List([Symbol("quote"); e])
  | Some('(') -> advance ctx;
                 skip_whitespace ctx;
                 let es = parse_many ctx in
                 expect_char ctx ')';
                 skip_whitespace ctx;
                 List(es)
  | Some(c) when is_symbolish c ->
      let s = parse_symbolish ctx !(snd ctx) in
      skip_whitespace ctx;
      (match Int64.of_string_opt s with
      | Some(n) -> Int(n)
      | None -> Symbol(s))
  | _ -> expectation_failed ctx "an expression"

and parse_many (ctx: string * int ref) : t list =
  let hd' : t option =
    try
      Some(parse_one ctx)
    with
      Parser_error(_) -> None
  in
  match hd' with
  | Some(hd) -> hd :: parse_many ctx
  | None -> []

let parse (src: string) : (t list, string) result =
  let ctx = (src, ref 0) in
  try
    skip_whitespace ctx;
    let es = parse_many ctx in
    expect_eof ctx;
    Ok(es)
  with
    Parser_error(s) -> Error(s)
