open Utils

type t
  = App
  | Fst
  | Snd
  | Id
  | Add
  | Mul
  | QuoteName of string
  | QuoteNum of int
  | Lam of t
  | Com of t * t
  | Pair of t * t

let rec to_string_prec : t -> int -> string = function
  | App -> const "app"
  | Fst -> const "fst"
  | Snd -> const "snd"
  | Id  -> const "id"
  | Add -> const "add"
  | Mul -> const "mul"
  | QuoteName(n) -> const ("'" ^ n)
  | QuoteNum(n)  -> const ("'" ^ string_of_int n)
  | Lam(b) -> const ("Λ(" ^ to_string_prec b 1 ^ ")")
  | Com(g, f) -> opt_parens (to_string_prec g 0 ^ " ∘ " ^ to_string_prec f 0) 1
  | Pair(l, r) -> const ("<" ^ to_string_prec l 1 ^ ", " ^ to_string_prec r 1 ^ ">")

let to_string (expr: t) : string =
  to_string_prec expr 1

module M = Monad.Writer_pure(Monad.Monoid_bool_or)

(* The flag is set if an optimization was performed. *)
let rec optimize1 : t -> t M.t =
  let open M in
  function
  (* The rewrite rules *)
  | Com(Fst, Pair(x, _)) ->      tell true >> return x
  | Com(Snd, Pair(_, y)) ->      tell true >> return y
  | Com(App, Pair(Lam(x), y)) -> tell true >> return (Com(x, Pair(Id, y)))
  (* Recursively apply the rewrite *)
  | App          -> return App
  | Fst          -> return Fst
  | Snd          -> return Snd
  | Id           -> return Id
  | Add          -> return Add
  | Mul          -> return Mul
  | QuoteName(n) -> return (QuoteName(n))
  | QuoteNum(n)  -> return (QuoteNum(n))
  | Lam(b)       -> let+ b' = optimize1 b in
                    return (Lam(b'))
  | Com(g, f)    -> let+ g' = optimize1 g
                    and+ f' = optimize1 f in
                    return (Com(g', f'))
  | Pair(l, r)   -> let+ l' = optimize1 l
                    and+ r' = optimize1 r in
                    return (Pair(l', r'))

let rec optimize (expr: t) : t =
  let (expr', rerun) = M.run (optimize1 expr) in
  if rerun then
    optimize expr'
  else
    expr'

let compile_to_forth (expr: t) : Forth.program =
  let defs = ref [] in
  let rec helper : t -> Forth.word list = function
  | App -> [Forth.App]
  | Fst -> [Forth.Fst]
  | Snd -> [Forth.Snd]
  | Id  -> []
  | Add -> [Forth.Add]
  | Mul -> [Forth.Mul]
  | QuoteName(n) -> [Forth.QuoteName(n)]
  | QuoteNum(n)  -> [Forth.QuoteNum(n)]
  | Lam(b) -> begin
                let b' = helper b
                and name = gensym ()
                in
                defs := (name, Forth.optimize b') :: !defs;
                [Forth.Lam(name)]
              end
  | Com(g, f) -> helper f @ helper g
  | Pair(l, r) -> [Forth.Dup] @ helper l @ [Forth.Swap] @ helper r @ [Forth.Cons]
  in let main = helper expr
  in { defs = !defs; main = Forth.optimize main }
