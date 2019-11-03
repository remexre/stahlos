open Uast
open Utils

type constraint_
  = Eq of expr_inner * expr_inner
  | Lub of expr_inner * expr_inner * expr_inner
  | Ty of expr_inner * expr_inner

let string_of_constraint = function
  | Eq(l, r) -> Printf.sprintf "%s = %s" (string_of_expr_inner l) (string_of_expr_inner r)
  | Lub(e, l, r) -> Printf.sprintf "%s = %s ^ %s" (string_of_expr_inner e)
      (string_of_expr_inner l) (string_of_expr_inner r)
  | Ty(e, t) -> Printf.sprintf "%s : %s" (string_of_expr_inner e) (string_of_expr_inner t)

let walk subst = function
  | Eq(l, r) -> Eq(walk_inner subst l, walk_inner subst r)
  | Lub(e, l, r) -> Lub(walk_inner subst e, walk_inner subst l, walk_inner subst r)
  | Ty(e, t) -> Ty(walk_inner subst e, walk_inner subst t)

module M = Monad.RWS_t(struct type t = (string * Tast.expr_inner) list end)
                      (Monad.Monoid_unit)
                      (struct type t = constraint_ list * subst end)
                      (Monad.Identity)
open M

let pushC c = modify (fun (cs, s) -> (c::cs, s))
let pushS var expr = modify (fun (cs, s) -> (cs, (var, expr) :: s))

exception Failed_to_solve of constraint_

let solve_one c =
  logf "About to solve %s\n" (string_of_constraint c);
  let+ (_, subst) = get in
  match walk subst c with
  (* Direct variable setting *)
  | Eq(Var(_, i), Var(_, j)) when i = j -> return ()
  | Eq(Var(_, i), j) -> pushS i j
  | Eq(i, Var(_, j)) -> pushS j i
  (* Simple type cases *)
  | Ty(Global(s), t) ->
      let+ ctx = ask in
      let t' = List.assoc s ctx in
      pushC (Eq(from_tast_inner t', t))
  | Ty(LitTy(_), t) -> pushC (Eq(Universe, t))
  (* Final fallbacks. *)
  | Eq(l, r) when l = r -> return ()
  | _ -> raise (Failed_to_solve(c))

let rec solve_all () =
  let+ (cs, s) = get in
  match cs with
  | [] -> return ()
  | hd::tl -> put (tl, s) >> solve_one hd >> solve_all ()

let solve ctx cs = let ((), (_, s), ()) = run (solve_all ()) ctx (cs, []) in s
