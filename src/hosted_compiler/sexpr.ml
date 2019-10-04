open Utils

type t
  = Int of Int64.t
  | List of t list
  | Symbol of string

let rec to_string : t -> string = function
  | Int(x) -> Int64.to_string x
  | List(l) -> "(" ^ join_with " " (List.map to_string l) ^ ")"
  | Symbol(s) -> s



module Parser = struct
  open Rse_monad
  open Parser_monad
  open Parsers

  let symbolish_char =
    let is_symbolish_char = function
    | c -> ('0' <= c && c <= '9') || ('A' <= c && c <= 'Z') || ('a' <= c && c <= 'z')
    in any_matching "A symbolish character" is_symbolish_char

  let symbolish =
    parse_while_s symbolish_char >>= fun s ->
    skip_whitespace >>= fun _ ->
    return (Symbol(s))

  let rec list () =
    terminal "(" >>= fun _ ->
    sexprs () >>= fun l ->
    terminal ")" >>= fun _ ->
    return (List(l))
  
  and sexpr () =
    list () <|> symbolish

  and sexprs () =
    parse_while (sexpr ())

  let parser =
    skip_whitespace >>= fun _ ->
    sexprs () >>= fun sexprs ->
    expect_eof >>= fun _ ->
    return sexprs
end

let parse : string -> (t list, string) result =
  Parser_monad.run Parser.parser
