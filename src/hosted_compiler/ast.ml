open Sexpr
open Utils

exception Invalid_ast of string * Sexpr.t

type expr
  = App of expr * expr
  | AppI of expr * expr
  | Global of string
  | Hole of string
  | Lam of string * expr
  | LamI of string * expr
  | Lit of Sexpr.t
  | Local of int
  | Pi of string * expr * expr
  | PiI of string * expr * expr
  | Universe

let check_name (name: string) : unit =
  let len = String.length name in
  let bad_builtin = len > 0 && String.contains (String.sub name 1 (len - 1)) '%'
  and empty = len = 0
  and hole_like = len > 0 && String.get name 0 = '_'
  and implicit_like = String.contains name '@'
  and keyword = List.mem name ["->"; "Pi"; "Type"; "fn"; "quote"]
  in
  if bad_builtin || empty || hole_like || implicit_like || keyword then
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

let rec decompose_lamis : expr -> string list * expr = function
  | LamI(n, b) ->
      let (args, expr) = decompose_lamis b in
      (n::args, expr)
  | e -> ([], e)

let rec decompose_pis : expr -> (string * expr) list * expr = function
  | Pi(n, t, b) ->
      let (args, expr) = decompose_pis b in
      ((n, t)::args, expr)
  | e -> ([], e)

let rec decompose_piis : expr -> (string * expr) list * expr = function
  | PiI(n, t, b) ->
      let (args, expr) = decompose_piis b in
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
  | List([Atom("Pi"); Atom(name); arg; ret]) ->
      check_name name;
      let arg = expr_of_sexpr ctx arg
      and ret = expr_of_sexpr (name::ctx) ret in
      Pi(name, arg, ret)
  | List([Atom("Pi"); List(args); ret]) ->
      let rec helper (ctx: string list) : Sexpr.t list -> expr = function
      | [] -> expr_of_sexpr ctx ret
      | List([Atom(name); ty])::tl ->
          check_name name;
          Pi(name, expr_of_sexpr ctx ty, helper (name::ctx) tl)
      | e::_ -> raise (Invalid_ast("Invalid argument", e))
      in helper ctx args
  | Atom("_") -> Hole("") (* TODO: Named holes? *)
  | Atom("Type") -> Universe
  | Atom(n) ->
      check_name n;
      let rec helper (i: int) : string list -> expr = function
      | [] -> Global(n)
      | hd::_ when hd = n -> Local(i)
      | _::tl -> helper (i+1) tl
      in helper 0 ctx
  | List([Atom("@"); e]) -> expr_of_sexpr ctx e
  | List(Atom("@")::f::xs) -> AppI(expr_of_sexpr ctx (List(Atom("@")::f::init xs)),
                                   expr_of_sexpr ctx (last xs))
  | List([e]) -> expr_of_sexpr ctx e
  | List(f::xs) -> App(expr_of_sexpr ctx (List(f::init xs)), expr_of_sexpr ctx (last xs))
  | e -> raise (Invalid_ast("Unrecognized expression", e))

let rec sexpr_of_expr : expr -> Sexpr.t = function
  | App(_, _) as e ->
      let (f, xs) = decompose_apps e in
      List(List.map sexpr_of_expr (f::xs))
  | AppI(f, x) -> List([Atom("@"); sexpr_of_expr f; sexpr_of_expr x])
  | Global(s) -> Atom(s)
  | Hole(s) -> Atom("_" ^ s)
  | Lam(_, _) as e ->
      let (args, body) = decompose_lams e in
      let atom s = Atom(s) in
      List([Atom("fn"); List(List.map atom args); sexpr_of_expr body])
  | LamI(_, _) as e ->
      let (args, body) = decompose_lamis e in
      let atom s = Atom(s) in
      List([Atom("fn@"); List(List.map atom args); sexpr_of_expr body])
  | Local(n) -> Atom("$" ^ string_of_int n)
  | Lit(Int(_) as e) -> e
  | Lit(String(_) as e) -> e
  | Lit(e) -> List([Atom("quote"); e])
  | Pi(_, _, _) as e ->
      let (args, body) = decompose_pis e in
      let sexpr_of_arg (s, e) = List([Atom(s); sexpr_of_expr e]) in
      List([Atom("Pi"); List(List.map sexpr_of_arg args); sexpr_of_expr body])
  | PiI(_, _, _) as e ->
      let (args, body) = decompose_piis e in
      let sexpr_of_arg (s, e) = List([Atom(s); sexpr_of_expr e]) in
      List([Atom("Pi@"); List(List.map sexpr_of_arg args); sexpr_of_expr body])
  | Universe -> Atom("Type")

let string_of_expr : expr -> string = Sexpr.to_string %% sexpr_of_expr

type defty =
  { name : string
  ; pargs : (string * expr) list
  ; iargs : (string * expr) list
  }

type ctor =
  { name : string
  ; args : (string * expr) list
  ; tyargs : expr list
  }

type def
  = Def of string * expr * expr
  | Deftype of defty * ctor list

let make_defty (name: Sexpr.t) (kind: Sexpr.t) : defty =
  let (name', pargs) = match name with
  | Atom(n) -> (n, [])
  | List(Atom(n)::args) ->
      let rec helper (ctx: string list) : Sexpr.t list -> (string * expr) list = function
      | [] -> []
      | List([Atom(n); t])::tl -> (n, expr_of_sexpr ctx t) :: helper (n::ctx) tl
      | _ -> raise (Invalid_ast("Invalid type name", name))
      in (n, helper [] args)
  | _ -> raise (Invalid_ast("Invalid type name", name))
  in
  let (iargs, kind') = decompose_pis (expr_of_sexpr [] kind) in
  if kind' <> Universe then
    raise (Invalid_ast("Invalid kind for defty", kind));
  { name = name'; iargs = iargs; pargs = pargs }

let make_ctor (def: defty) (name: string) (ty: Sexpr.t) : ctor =
  let ctx = List.map fst def.pargs in
  assert (def.iargs = []);
  let (args, ty') = decompose_pis (expr_of_sexpr ctx ty) in
  let (ty', tyargs) = decompose_apps ty' in
  Utils.logln "ctor";
  Utils.logln ("  def.name: " ^ def.name);
  Utils.logln ("  def.pargs: [" ^ join_with ", " (List.map (fun (s, e) -> s ^ ":" ^ string_of_expr e) def.pargs) ^ "]");
  Utils.logln ("  def.iargs: [" ^ join_with ", " (List.map (fun (s, e) -> s ^ ":" ^ string_of_expr e) def.iargs) ^ "]");
  Utils.logln ("  name: " ^ name);
  Utils.logln ("  ty: " ^ string_of_expr ty');
  Utils.logln ("  args: [" ^ join_with ", " (List.map (fun (s, e) -> s ^ ":" ^ string_of_expr e) args) ^ "]");
  Utils.logln ("  tyargs: [" ^ join_with ", " (List.map string_of_expr tyargs) ^ "]");
  Utils.logln "---------";
  if ty' <> Global(def.name) then
    raise (Invalid_ast("Invalid return type for constructor", ty));
  { name = name; args = args; tyargs = tyargs }

let rec make_ctors (def: defty) : Sexpr.t list -> ctor list = function
  | [] -> []
  | [e] -> raise (Invalid_ast("Odd number of arguments to deftype", e))
  | (Atom(name))::ty::tl ->
      check_name name;
      make_ctor def name ty :: make_ctors def tl
  | e::_ -> raise (Invalid_ast("Invalid name in deftype", e))

let def_of_sexpr : Sexpr.t -> def = function
  | List([Atom("def"); Atom(name); ty; expr]) ->
      check_name name;
      Def(name, expr_of_sexpr [] ty, expr_of_sexpr [] expr)
  | List(Atom("deftype")::name_and_indexed::kind::ctors) ->
      let def = make_defty name_and_indexed kind in
      Deftype(def, make_ctors def ctors)
  | List([Atom("defun"); Atom(name); List(args); ret_ty; expr]) ->
      check_name name;
      let rec helper (has_implicits: bool) (ctx: string list) : Sexpr.t list -> expr * expr = function
      | [] -> (expr_of_sexpr ctx ret_ty, expr_of_sexpr ctx expr)
      | Atom("@")::tl ->
          if has_implicits then
            helper false ctx tl
          else
            raise (Invalid_ast("Unexpected @", List(args)))
      | List([Atom(n); ty])::tl ->
          check_name n;
          let (ty', ex') = helper has_implicits (n::ctx) tl in
          if has_implicits then
            (PiI(n, expr_of_sexpr ctx ty, ty'), LamI(n, ex'))
          else
            (Pi(n, expr_of_sexpr ctx ty, ty'), Lam(n, ex'))
      | e::_ -> raise (Invalid_ast("Invalid argument", e))
      in
      let has_implicits = List.mem (Atom("@")) args in
      let (ty, ex) = helper has_implicits [] args in
      Def(name, ty, ex)
  | e -> raise (Invalid_ast("Unrecognized definition", e))

let sexprs_of_defty (dt: defty) : Sexpr.t * expr =
  assert (dt.iargs = []);
  let name = List.fold_right (fun (n, t) e -> Pi(n, t, e)) dt.pargs (Global(dt.name)) in
  let name = match sexpr_of_expr name with
  | Atom(n) -> Atom(n)
  | List([Atom("Pi"); List(args); name]) -> List(name::args)
  | _ -> failwith "unreachable"
  in
  (name, Universe)

let sexprs_of_ctor (dt: defty) (ctor: ctor) : expr * expr =
  assert (dt.iargs = []);
  let ty = List.fold_right (fun t e -> App(e, t)) ctor.tyargs (Global(dt.name)) in
  let ty = List.fold_right (fun (n, t) e -> Pi(n, t, e)) ctor.args ty in
  (Global(ctor.name), ty)

let sexpr_of_def : def -> Sexpr.t = function
  | Def(n, t, e) -> List([Atom("def"); Atom(n); sexpr_of_expr t; sexpr_of_expr e])
  | Deftype(dt, cs) ->
      let rec helper = function
      | [] -> []
      | hd::tl ->
          let (a, b) = sexprs_of_ctor dt hd in
          a::b::helper tl
      in
      let (a, b) = sexprs_of_defty dt in
      List(Atom("deftype")::a::(List.map sexpr_of_expr (b::helper cs)))

let string_of_def : def -> string = Sexpr.to_string %% sexpr_of_def

type module_ =
  { name : string
  ; defs : def list
  }

let module_of_sexprs : Sexpr.t list -> module_ = function
  | List([Atom("module"); Atom(name)])::defs ->
      check_name name;
      { name = name; defs = List.map def_of_sexpr defs }
  | [] -> raise (Invalid_ast("Missing module form", List([])))
  | hd::_ -> raise (Invalid_ast("Invalid module form", hd))

let sexprs_of_module (m: module_) : Sexpr.t list =
  let module_form = List([Atom("module"); Atom(m.name)])
  and defs = List.map sexpr_of_def m.defs in
  module_form::defs

let string_of_module : module_ -> string =
  join_with "\n" %% List.map Sexpr.to_string %% sexprs_of_module

let load_module_from : in_channel -> module_ =
  module_of_sexprs %% must id %% Sexpr.parse %% read_all_string

let load_module_from_path : string -> module_ =
  module_of_sexprs %% must id %% Sexpr.parse %% read_file_string
