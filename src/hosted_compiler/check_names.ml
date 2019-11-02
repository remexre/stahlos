open Utils

exception Invalid_defined_name of string * Ast.def
exception Invalid_name of string
exception Redefined_name of string * Ast.def
exception Unbound_name of string * Ast.expr list

let for_module (m: Ast.module_) : unit =
  let names = Hashtbl.create 128 in
  List.iter (fun (n, _) -> Hashtbl.add names n ()) Builtins.builtins;
  let find_name (n: string) : unit =
    if n = "" then
      raise (Invalid_name(n));
    if not (Hashtbl.mem names n) then
      raise (Unbound_name(n, []))
  in
  let register_name (def: Ast.def) (n: string) : unit =
    if n = "" then
      raise (Invalid_defined_name(n, def));
    if String.get n 0 = '%' then
      raise (Invalid_defined_name(n, def));
    if insert_and_get names n then
      raise (Redefined_name(n, def))
  in
  let rec for_expr (e: Ast.expr) : unit =
    try
      match e with
      | App(f, x) -> for_expr f; for_expr x
      | AppI(f, x) -> for_expr f; for_expr x
      | Global(n) -> find_name n
      | Hole(_) -> ()
      | Lam(_, b) -> for_expr b
      | LamI(_, b) -> for_expr b
      | Lit(_) -> ()
      | Local(_) -> ()
      | Pi(_, t, b) -> for_expr t; for_expr b
      | PiI(_, t, b) -> for_expr t; for_expr b
      | Universe -> ()
    with
      Unbound_name(n, es) -> raise (Unbound_name(n, e::es))
  and register_ctor (def: Ast.def) (c: Ast.ctor) : unit =
    register_name def c.name
  and register_defty (def: Ast.def) (dt: Ast.defty) : unit =
    register_name def ("elim-" ^ dt.name)
  and check_ctor (c: Ast.ctor) : unit =
    List.iter (for_expr %% snd) c.args;
    List.iter (for_expr) c.tyargs
  and check_defty (dt: Ast.defty) : unit =
    List.iter (for_expr %% snd) dt.pargs;
    List.iter (for_expr %% snd) dt.iargs
  in
  let for_def (def: Ast.def) : unit = match def with
  | Def(n, t, e) ->
      for_expr t;
      for_expr e;
      register_name def n
  | Deftype(dt, cs) ->
      check_defty dt;
      register_name def dt.name;
      List.iter check_ctor cs;
      register_defty def dt;
      List.iter (register_ctor def) cs
  in
  List.iter for_def m.defs
