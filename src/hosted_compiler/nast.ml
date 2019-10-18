exception Unbound_name of string * Ast.expr list

type expr
  = App of expr * expr
  | Global of string
  | Hole
  | Lam of string * expr
  | Lit of Sexpr.t
  | Local of int
  | Pi of string * expr * expr
  | Universe

let rec resolve_names_for_expr defs depth expr =
  try
    match expr with
    | Ast.App(f, x) -> App(resolve_names_for_expr defs 0 f,
                           resolve_names_for_expr defs 0 x)
    | Ast.AppI(f, x) -> App(resolve_names_for_expr defs (depth+1) f,
                            resolve_names_for_expr defs 0 x)
    | Ast.Global(n) ->
        begin
          let rec add_holes expr n =
            if n <= 0 then
              expr
            else
              add_holes (App(expr, Hole)) (n-1)
          in
          match List.assoc_opt n defs with
          | Some(wanted) -> add_holes (Global(n)) (wanted - depth)
          | None -> raise (Unbound_name(n, []))
        end
    | Ast.Hole -> Hole
    | Ast.Lam(n, b) | Ast.LamI(n, b) -> Lam(n, resolve_names_for_expr defs 0 b)
    | Ast.Lit(l) -> Lit(l)
    | Ast.Local(n) -> Local(n)
    | Ast.Pi(n, t, b) | Ast.PiI(n, t, b) -> Pi(n, resolve_names_for_expr defs 0 t,
                                               resolve_names_for_expr defs 0 b)
    | Ast.Universe -> Universe
  with
    Unbound_name(n, es) -> raise (Unbound_name(n, expr::es))

type defty =
  { name : string
  ; implicits : int
  ; args : (string * expr) list
  }

type ctor =
  { name : string
  ; args : (string * expr) list
  ; tyargs : expr list
  }

type def
  = Def of string * int * expr * expr
  | Deftype of defty * ctor list

let rec count_pii = function
  | Ast.PiI(_, _, b) -> 1 + count_pii b
  | _ -> 0

let resolve_names_for_def defs = function
  | Ast.Def(n, t, e) -> let is = count_pii t in
                        let def = Def(n, is, resolve_names_for_expr defs 0 t,
                                      resolve_names_for_expr defs 0 e) in
                        (def, [(n, is)])
  | Ast.Deftype(dt, cs) ->
      let dt_ni = List.length dt.iargs in
      let dt' = { name = dt.name; implicits = dt_ni
                ; args = List.map (fun (n, e) -> (n, resolve_names_for_expr defs 0 e))
                                  (dt.iargs @ dt.pargs)
                } in
      let defs = (dt.name, dt_ni)::defs in
      let cs_helper (ctor: Ast.ctor) =
        { name = ctor.name
        ; args = List.map (fun (n, e) -> (n, resolve_names_for_expr defs 0 e)) ctor.args
        ; tyargs = List.map (resolve_names_for_expr defs 0) ctor.tyargs
        } in
      let cs' = List.map cs_helper cs in
      (Deftype(dt', cs'), (dt.name, dt_ni)::(List.map (fun c -> (c.name, 0)) cs'))

type module_ =
  { name : string
  ; defs : def list
  }

let rec resolve_names_for_defs defs def_implicit_counts = function
  | [] -> List.rev defs
  | hd::tl -> let (hd', ics) = resolve_names_for_def def_implicit_counts hd in
              resolve_names_for_defs (hd'::defs) (ics@def_implicit_counts) tl

let resolve_names_for_module (m: Ast.module_) : module_ =
  let is = List.map (fun (n, _) -> (n, 0)) Builtins.builtins in
  { name = m.name; defs = resolve_names_for_defs [] is m.defs }
