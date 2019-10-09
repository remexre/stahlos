open Sexpr
open Utils

exception Invalid_ast of string * Sexpr.t

type expr
  = App of expr * expr
  | Global of string
  | Hole
  | Lam of string * expr
  | Lit of Sexpr.t
  | Local of int
  | Pi of string * expr * expr
  | Universe

let check_name (name: string) : unit =
  let keywords = ["->"; "Type"; "fn"; "pi"; "quote"] in
  if String.length name = 0 || String.get name 0 = '_' || List.mem name keywords then
    raise (Invalid_ast("Invalid name", Atom(name)))

let decompose_apps (expr: expr) : expr * expr list =
  let rec helper : expr -> expr * expr list = function
  | App(f, x) -> let (f', xs) = helper f in (f', x::xs)
  | e -> (e, [])
  in
  let (f, xs) = helper expr in
  (f, List.rev xs)

let rec decompose_lams : expr -> string list * expr = function
  | Lam(n, b) ->
      let (args, expr) = decompose_lams b in
      (n::args, expr)
  | e -> ([], e)

let rec decompose_pis : expr -> (string * expr) list * expr = function
  | Pi(n, t, b) ->
      let (args, expr) = decompose_pis b in
      ((n, t)::args, expr)
  | e -> ([], e)

let rec expr_of_sexpr (ctx: string list) : Sexpr.t -> expr = function
  | (Int(_) as e)
  | (String(_) as e)
  | List([Atom("quote"); e]) -> Lit(e)
  | List(Atom("quote")::_) as e -> raise (Invalid_ast("Invalid use of quote", e))
  | List([Atom("fn"); Atom(arg); body]) ->
      check_name arg;
      let body = expr_of_sexpr (arg::ctx) body in
      Lam(arg, body)
  | List([Atom("fn"); List(args); body]) ->
      let check_arg_name : Sexpr.t -> string = function
      | Atom(name) ->
          check_name name;
          name
      | e -> raise (Invalid_ast("Invalid argument name", e))
      in
      let rec helper (ctx: string list) : string list -> expr = function
      | [] -> expr_of_sexpr ctx body
      | hd::tl -> Lam(hd, helper (hd::ctx) tl)
      in helper ctx (List.map check_arg_name args)
  | List(Atom("->")::((_::_) as tys)) ->
      let tys = List.map (expr_of_sexpr ctx) tys in
      let ret = last tys in
      let rec helper : expr list -> expr = function
      | [] -> ret
      | hd::tl -> Pi(gensym (), hd, helper tl)
      in helper (init tys)
  | List([Atom("pi"); Atom(name); arg; ret]) ->
      check_name name;
      let arg = expr_of_sexpr ctx arg
      and ret = expr_of_sexpr (name::ctx) ret in
      Pi(name, arg, ret)
  | List([Atom("pi"); List(args); ret]) ->
      let rec helper (ctx: string list) : Sexpr.t list -> expr = function
      | [] -> expr_of_sexpr ctx ret
      | List([Atom(name); ty])::tl ->
          check_name name;
          Pi(name, expr_of_sexpr ctx ty, helper (name::ctx) tl)
      | e::_ -> raise (Invalid_ast("Invalid argument", e))
      in helper ctx args
  | Atom("_") -> Hole
  | Atom("Type") -> Universe
  | Atom(n) ->
      check_name n;
      let rec helper (i: int) : string list -> expr = function
      | [] -> Global(n)
      | hd::_ when hd = n -> Local(i)
      | _::tl -> helper (i+1) tl
      in helper 0 ctx
  | List([e]) -> expr_of_sexpr ctx e
  | List(f::xs) -> App(expr_of_sexpr ctx (List(f::init xs)), expr_of_sexpr ctx (last xs))
  | e -> raise (Invalid_ast("Unrecognized expression", e))

let rec sexpr_of_expr : expr -> Sexpr.t = function
  | App(_, _) as e ->
      let (f, xs) = decompose_apps e in
      List(List.map sexpr_of_expr (f::xs))
  | Global(s) -> Atom(s)
  | Hole -> Atom("_")
  | Lam(_, _) as e ->
      let (args, body) = decompose_lams e in
      let atom s = Atom(s) in
      List([Atom("fn"); List(List.map atom args); sexpr_of_expr body])
  | Local(n) -> Atom("$" ^ string_of_int n)
  | Lit(Int(_) as e) -> e
  | Lit(String(_) as e) -> e
  | Lit(e) -> List([Atom("quote"); e])
  | Pi(_, _, _) as e ->
      let (args, body) = decompose_pis e in
      let sexpr_of_arg (s, e) = List([Atom(s); sexpr_of_expr e]) in
      List([Atom("pi"); List(List.map sexpr_of_arg args); sexpr_of_expr body])
  | Universe -> Atom("Type")

let string_of_expr : expr -> string = Sexpr.to_string %% sexpr_of_expr

type defty =
  { name : string
  ; iargs : (string * expr) list
  ; pargs : expr list
  }

type ctor =
  { name : string
  ; args_tys : expr list
  ; tyarg_tys : expr list
  }

type def
  = Def of string * expr * expr
  | Deftype of defty * ctor list

let make_defty (name: Sexpr.t) (kind: expr) : defty =
  let _ = name
  and _ = kind
  in failwith "TODO make_defty"

let make_ctor (name: string) (ty: Sexpr.t) : ctor =
  let _ = (name, ty) in
  (* { name = name; } *)
  failwith "TODO make_ctor"

let rec make_ctors : Sexpr.t list -> ctor list = function
  | [] -> []
  | [e] -> raise (Invalid_ast("Odd number of arguments to deftype", e))
  | (Atom(name))::ty::tl ->
      check_name name;
      make_ctor name ty :: make_ctors tl
  | e::_ -> raise (Invalid_ast("Invalid name in deftype", e))

let def_of_sexpr : Sexpr.t -> def = function
  | List([Atom("def"); Atom(name); ty; expr]) ->
      check_name name;
      Def(name, expr_of_sexpr [] ty, expr_of_sexpr [] expr)
  | List(Atom("deftype")::name_and_indexed::kind::ctors) ->
      Deftype(make_defty name_and_indexed (expr_of_sexpr [] kind), make_ctors ctors)
  | List([Atom("defun"); Atom(name); List(args); ret_ty; expr]) ->
      check_name name;
      let rec helper (ctx: string list) : Sexpr.t list -> expr * expr = function
      | [] -> (expr_of_sexpr ctx ret_ty, expr_of_sexpr ctx expr)
      | List([Atom(n); ty])::tl ->
          check_name n;
          let (ty', ex') = helper (n::ctx) tl in
          (Pi(n, expr_of_sexpr ctx ty, ty'), Lam(n, ex'))
      | e::_ -> raise (Invalid_ast("Invalid argument", e))
      in
      let (ty, ex) = helper [] args in
      Def(name, ty, ex)
  | e -> raise (Invalid_ast("Unrecognized definition", e))

let sexpr_of_def : def -> Sexpr.t = function
  | Def(n, t, e) -> List([Atom("def"); Atom(n); sexpr_of_expr t; sexpr_of_expr e])
  | Deftype(_, _) -> failwith "TODO sexpr_of_def deftype"

let string_of_def : def -> string = Sexpr.to_string %% sexpr_of_def
