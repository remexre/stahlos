open Sexpr
open Utils

exception Invalid_ast of string * Sexpr.t

type expr
  = App of expr * expr
  | Hole
  | Lam of (expr -> expr)
  | Lit of Sexpr.t
  | Pi of expr * (expr -> expr)
  | Universe
  | Var of string

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

let rec subst (n: string) (r: expr) : expr -> expr = function
  | App(f, x) -> App(subst n r f, subst n r x)
  | Hole -> Hole
  | Lam(b) -> Lam(fun x -> subst n r (b x))
  | Lit(l) -> Lit(l)
  | Pi(t, b) -> Pi(subst n r t, fun x -> subst n r (b x))
  | Universe -> Universe
  | Var(s) when s = n -> r
  | Var(s) -> Var(s)

let rec expr_of_sexpr : Sexpr.t -> expr = function
  | (Int(_) as e)
  | (String(_) as e)
  | List([Atom("quote"); e]) -> Lit(e)
  | List(Atom("quote")::_) as e -> raise (Invalid_ast("Invalid use of quote", e))
  | List([Atom("fn"); Atom(arg); body]) ->
      check_name arg;
      let body = expr_of_sexpr body in
      Lam(fun x -> subst arg x body)
  | List([Atom("fn"); List(args); body]) ->
      let check_arg_name : Sexpr.t -> string = function
      | Atom(name) ->
          check_name name;
          name
      | e -> raise (Invalid_ast("Invalid argument name", e))
      and body = expr_of_sexpr body in
      let rec helper : string list -> expr = function
      | [] -> body
      | hd::tl -> Lam(fun x -> subst hd x (helper tl))
      in helper (List.map check_arg_name args)
  | List(Atom("->")::((_::_) as tys)) ->
      let tys = List.map expr_of_sexpr tys in
      let ret = last tys in
      let rec helper : expr list -> expr = function
      | [] -> ret
      | hd::tl -> Pi(hd, fun _ -> helper tl)
      in helper (init tys)
  | List([Atom("pi"); Atom(name); arg; ret]) ->
      check_name name;
      let arg = expr_of_sexpr arg
      and ret = expr_of_sexpr ret in
      Pi(arg, fun x -> subst name x ret)
  | List([Atom("pi"); List(args); ret]) -> make_pi args ret
  | Atom("_") -> Hole
  | Atom("Type") -> Universe
  | Atom(n) -> check_name n; Var(n)
  | List([e]) -> expr_of_sexpr e
  | List(f::xs) -> App(expr_of_sexpr (List(f::init xs)), expr_of_sexpr (last xs))
  | e -> raise (Invalid_ast("Unrecognized expression", e))

and make_args (args: Sexpr.t list) : (string * expr) list =
  let make_pi_arg : Sexpr.t -> string * expr = function
  | List([Atom(name); expr]) ->
      check_name name;
      (name, expr_of_sexpr expr)
  | e -> raise (Invalid_ast("Invalid argument", e))
  in List.map make_pi_arg args

and make_pi (args: Sexpr.t list) (ret_ty: Sexpr.t) : expr =
  let rec loop : (string * expr) list -> expr = function
  | [] -> expr_of_sexpr ret_ty
  | (n, t)::tl -> Pi(t, fun x -> subst n x (loop tl))
  in loop (make_args args)

let rec sexpr_of_expr : expr -> Sexpr.t = function
  | App(_, _) as e ->
      let (f, xs) = decompose_apps e in
      List(List.map sexpr_of_expr (f::xs))
  | Hole -> Atom("_")
  | Lam(b) ->
      let name = gensym () in
      List([Atom("fn"); Atom(name); sexpr_of_expr (b (Var(name)))])
  | Lit(Int(_) as e) -> e
  | Lit(String(_) as e) -> e
  | Lit(e) -> List([Atom("quote"); e])
  | Pi(t, b) ->
      let name = gensym () in
      List([Atom("pi"); Atom(name); sexpr_of_expr t; sexpr_of_expr (b (Var(name)))])
  | Universe -> Atom("Type")
  | Var(s) -> Atom(s)

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
  | [e] -> raise (Invalid_ast("Extra argument to deftype", e))
  | (Atom(name))::ty::tl ->
      check_name name;
      make_ctor name ty :: make_ctors tl
  | e::_ -> raise (Invalid_ast("Invalid name in deftype", e))

let def_of_sexpr : Sexpr.t -> def = function
  | List([Atom("def"); Atom(name); ty; expr]) ->
      check_name name;
      Def(name, expr_of_sexpr ty, expr_of_sexpr expr)
  | List(Atom("deftype")::name_and_indexed::kind::ctors) ->
      Deftype(make_defty name_and_indexed (expr_of_sexpr kind), make_ctors ctors)
  | List([Atom("defun"); Atom(name); List(args); ret_ty; expr]) ->
      check_name name;
      let rec helper : (string * expr) list -> expr = function
      | [] -> expr_of_sexpr expr
      | (n, _)::tl -> Lam(fun x -> subst n x (helper tl))
      in Def(name, make_pi args ret_ty, helper (make_args args))
  | e -> raise (Invalid_ast("Unrecognized definition", e))

let sexpr_of_def : def -> Sexpr.t = function
  | Def(n, t, e) -> List([Atom("def"); Atom(n); sexpr_of_expr t; sexpr_of_expr e])
  | Deftype(_, _) -> failwith "TODO sexpr_of_def deftype"

let string_of_def : def -> string = Sexpr.to_string %% sexpr_of_def
