(*********)
(* utils *)
(*********)

let id (x: 'a) : 'a = x

(****************************)
(* Our lambda calculus term *)
(****************************)

type lam
  = Abs of lam
  | Add of lam * lam
  | App of lam * lam
  | Con of int
  | Mul of lam * lam
  | Var of int

let ki12 = App(App(App(Abs(Abs(Var(1))), Abs(Var(0))), Con(1)), Con(2))
let k12 = App(App(Abs(Abs(Var(1))), Con(1)), Con(2))
let id3 = App(Abs(App(Abs(Var(0)), Var(0))), Con(3))
let add23 = Add(Con(2), Con(3))

(**********************)
(* Static combinators *)
(**********************)

type cam
  = SApp
  | SFst
  | SSnd
  | SAdd
  | SMul
  | SQuote of int
  | SLam of cam
  | SCom of cam * cam
  | SPair of cam * cam
  | SId (* only used later on, in optimization *)

let rec cam_of_lam : lam -> cam = function
  | Abs(b) -> SLam(cam_of_lam b)
  | Add(x, y) -> SCom(SAdd, SPair(cam_of_lam x, cam_of_lam y))
  | App(f, x) -> SCom(SApp, SPair(cam_of_lam f, cam_of_lam x))
  | Con(s) -> SQuote(s)
  | Mul(x, y) -> SCom(SMul, SPair(cam_of_lam x, cam_of_lam y))
  | Var(n) -> if n < 0 then
                failwith "Invalid de Brujin index"
              else if n = 0 then
                SSnd
              else
                SCom(cam_of_lam(Var(n-1)), SFst)

let static_ki12 =
  let app x y = SCom(SApp, SPair(x, y)) in
  app (app (app (SLam(SLam(SCom(SSnd, SFst)))) (SLam(SSnd))) (SQuote(1))) (SQuote(2))

let static_k12 =
  let app x y = SCom(SApp, SPair(x, y)) in
  app (app (SLam(SLam(SCom(SSnd, SFst)))) (SQuote(1))) (SQuote(2))

let static_id3 =
  let app x y = SCom(SApp, SPair(x, y)) in
  app (SLam(app (SLam(SSnd)) SSnd)) (SQuote(3))

let static_add23 =
  SCom(SAdd, SPair(SQuote(2), SQuote(3)))

;;
(* wtf ocaml... the ;; above seems to be necessary... *)

assert (cam_of_lam ki12 = static_ki12);;
assert (cam_of_lam k12 = static_k12);;
assert (cam_of_lam id3 = static_id3);;
assert (cam_of_lam add23 = static_add23)

(********************************)
(* Static + dynamic combinators *)
(********************************)

type cam_runtime
  = CApp
  | CFst
  | CSnd
  | CAdd
  | CMul
  | CQuote of int
  | CLam of cam_runtime
  | CCom of cam_runtime * cam_runtime
  | CPair of cam_runtime * cam_runtime
  | CId
  (* Dynamic things below *)
  | CEmpty
  | CCon of int
  | CApply of cam_runtime * cam_runtime
  | CDPair of cam_runtime * cam_runtime

let rec inj : cam -> cam_runtime = function
  | SApp -> CApp
  | SFst -> CFst
  | SSnd -> CSnd
  | SAdd -> CAdd
  | SMul -> CMul
  | SQuote(s) -> CQuote(s)
  | SLam(b) -> CLam(inj b)
  | SCom(f, g) -> CCom(inj f, inj g)
  | SPair(l, r) -> CPair(inj l, inj r)
  | SId -> CId

let rec eval_cam_1 : cam_runtime -> cam_runtime = function
  (* standard rules *)
  | CApply(CCom(x, y), z) -> CApply(x, CApply(y, z))
  | CApply(CFst, CDPair(x, _)) -> x
  | CApply(CSnd, CDPair(_, y)) -> y
  | CApply(CPair(x, y), z) -> CDPair(CApply(x, z), CApply(y, z))
  | CApply(CApp, CDPair(CApply(CLam(x), y), z)) -> CApply(x, CDPair(y, z))
  | CApply(CQuote(s), _) -> CCon(s)
  (* arithmetic operators are currently curried... *)
  | CApply(CAdd, CDPair(CCon(x), CCon(y))) -> CCon(x + y)
  | CApply(CMul, CDPair(CCon(x), CCon(y))) -> CCon(x * y)
  (* apply them recursively *)
  | CApp -> CApp
  | CFst -> CFst
  | CSnd -> CSnd
  | CAdd -> CAdd
  | CMul -> CMul
  | CQuote(s) -> CQuote(s)
  | CLam(b) -> CLam(eval_cam_1 b)
  | CCom(f, g) -> CCom(eval_cam_1 f, eval_cam_1 g)
  | CPair(l, r) -> CPair(eval_cam_1 l, eval_cam_1 r)
  | CId -> CId
  (* also on the dynamic combinators *)
  | CEmpty -> CEmpty
  | CCon(s) -> CCon(s)
  | CApply(f, x) -> CApply(eval_cam_1 f, eval_cam_1 x)
  | CDPair(l, r) -> CDPair(eval_cam_1 l, eval_cam_1 r)

let rec eval_cam' (expr: cam_runtime) : cam_runtime =
  let next = eval_cam_1 expr in
  if expr = next then
    expr
  else
    eval_cam' next

let eval_cam (expr: cam) : cam_runtime =
  eval_cam' (CApply(inj expr, CEmpty));;

assert (eval_cam (cam_of_lam ki12) = CCon(2));;
assert (eval_cam (cam_of_lam k12) = CCon(1));;
assert (eval_cam (cam_of_lam id3) = CCon(3));;
assert (eval_cam (cam_of_lam add23) = CCon(5))

(*********************************)
(* High-level stack instructions *)
(*********************************)

type insn
  = IApp
  | ILam of insn list
  | IFst
  | ISnd
  | ICons
  | IPush
  | IQuote of int
  | IAdd
  | IMul
  | ISwap

let rec insns_of_cam : cam -> insn list = function
  | SApp -> [IApp]
  | SFst -> [IFst]
  | SSnd -> [ISnd]
  | SAdd -> [IAdd]
  | SMul -> [IMul]
  | SQuote(s) -> [IQuote(s)]
  | SLam(b) -> [ILam(insns_of_cam b)]
  | SCom(f, g) -> insns_of_cam g @ insns_of_cam f
  | SPair(l, r) -> [IPush] @ insns_of_cam l @ [ISwap] @ insns_of_cam r @ [ICons]
  | SId -> []

(*********************)
(* Runtime for insns *)
(*********************)

type insn_term
  = IClo of insn list * insn_term
  | ICon of int
  | IPair of insn_term * insn_term
  | IEmpty

type insn_state =
  { term : insn_term
  ; code : insn list
  ; stack : insn_term list
  }

type insn_status
  = Continue of insn_state
  | Done of insn_term * insn_term list

let rec step_insn_state (state: insn_state) : insn_status =
  let tm = state.term
  and st = state.stack in
  match state.code with
  | [] -> Done(state.term, state.stack)
  | (IApp::ctl) -> (match tm with
                   | IPair(IClo(b, s), t) -> Continue({term=IPair(s, t); code=b@ctl; stack=st})
                   | _ -> failwith "Type error at IApp")
  | (ILam(b)::ctl) -> Continue({term=IClo(b, tm); code=ctl; stack=st})
  | (IFst::ctl) -> (match tm with
                   | IPair(s, _) -> Continue({term=s; code=ctl; stack=st})
                   | _ -> failwith "Type error at IFst")
  | (ISnd::ctl) -> (match tm with
                   | IPair(_, t) -> Continue({term=t; code=ctl; stack=st})
                   | _ -> failwith "Type error at ISnd")
  | (ICons::ctl) -> (match st with
                    | shd::stl -> Continue({term=IPair(shd, tm); code=ctl; stack=stl})
                    | [] -> failwith "Stack underflow at IPair")
  | (IPush::ctl) -> Continue({term=tm; code=ctl; stack=(tm::st)})
  | (IQuote(s)::ctl) -> Continue({term=ICon(s); code=ctl; stack=st})
  | (IAdd::ctl) -> (match tm with
                   | IPair(ICon(x), ICon(y)) -> Continue({term=ICon(x + y); code=ctl; stack=st})
                   | _ -> failwith "Type error at ISnd")
  | (IMul::ctl) -> (match tm with
                   | IPair(ICon(x), ICon(y)) -> Continue({term=ICon(x * y); code=ctl; stack=st})
                   | _ -> failwith "Type error at ISnd")
  | (ISwap::ctl) -> (match st with
                    | shd::stl -> Continue({term=shd; code=ctl; stack=(tm::stl)})
                    | [] -> failwith "Stack underflow at ISwap")

let eval_insns' (insns: insn list) : insn_term * insn_term list =
  let rec helper (state: insn_state) : insn_term * insn_term list =
    match step_insn_state state with
    | Continue(state) -> helper state
    | Done(term, stack) -> (term, stack)
  in helper {term = IEmpty; code = insns; stack = []}

let eval_insns (insns: insn list) : insn_term =
  match eval_insns' insns with
  | (out, []) -> out
  | _ -> failwith "nonempty stack";;

assert (eval_insns (insns_of_cam (cam_of_lam ki12)) = ICon(2));;
assert (eval_insns (insns_of_cam (cam_of_lam k12)) = ICon(1));;
assert (eval_insns (insns_of_cam (cam_of_lam id3)) = ICon(3));;
assert (eval_insns (insns_of_cam (cam_of_lam add23)) = ICon(5))

(****************)
(* Optimization *)
(****************)

type 'a flag_monad = { run_flag_monad : 'a * bool }

let return (x: 'a) : 'a flag_monad =
  { run_flag_monad = (x, false) }
let (>>=) (x: 'a flag_monad) (f: 'a -> 'b flag_monad) : 'b flag_monad =
  let (x', flag1) = x.run_flag_monad in
  let (y, flag2) = (f x').run_flag_monad in
  { run_flag_monad = (y, flag1 || flag2) }
let (>>) (x: 'a flag_monad) (y: 'b flag_monad) : 'b flag_monad =
  x >>= fun _ -> y

let set : unit flag_monad =
  { run_flag_monad = ((), true) }

(* The flag is set if an optimization was performed. *)
let rec opt_cam_1 : cam -> cam flag_monad = function
  (* The rewrite rules *)
  | SCom(SFst, SPair(x, _)) ->       set >> return x
  | SCom(SSnd, SPair(_, y)) ->       set >> return y
  | SCom(SApp, SPair(SLam(x), y)) -> set >> return (SCom(x, SPair(SId, y)))
  (* Recursively apply the rewrite *)
  | SApp        -> return SApp
  | SFst        -> return SFst
  | SSnd        -> return SSnd
  | SAdd        -> return SAdd
  | SMul        -> return SMul
  | SQuote(s)   -> return (SQuote(s))
  | SLam(b)     -> opt_cam_1 b >>= fun b'
                -> return (SLam(b'))
  | SCom(f, g)  -> opt_cam_1 f >>= fun f'
                -> opt_cam_1 g >>= fun g'
                -> return (SCom(f', g'))
  | SPair(l, r) -> opt_cam_1 l >>= fun l'
                -> opt_cam_1 r >>= fun r'
                -> return (SPair(l', r'))
  | SId         -> return SId

let rec opt_cam (expr: cam) : cam =
  let (expr', rerun) = (opt_cam_1 expr).run_flag_monad in
  if rerun then
    opt_cam expr'
  else
    expr';;

let rec opt_insns : insn list -> insn list = function
  (* Optimize no-op sequences *)
  | IPush::ISwap::tl -> opt_insns (IPush::tl)
  (* Recursively optimize *)
  | [] -> []
  | hd::tl -> hd::opt_insns tl;;

assert (eval_cam (opt_cam (cam_of_lam ki12)) = CCon(2));;
assert (eval_cam (opt_cam (cam_of_lam k12)) = CCon(1));;
assert (eval_cam (opt_cam (cam_of_lam id3)) = CCon(3));;
assert (eval_cam (opt_cam (cam_of_lam add23)) = CCon(5))

let size_after f g expr = List.length (f (insns_of_cam (g (cam_of_lam expr))));;

assert (size_after id        opt_cam ki12  <= size_after id id ki12);;
assert (size_after opt_insns opt_cam ki12  <= size_after id id ki12);;
assert (size_after id        opt_cam k12   <= size_after id id k12);;
assert (size_after opt_insns opt_cam k12   <= size_after id id k12);;
assert (size_after id        opt_cam id3   <= size_after id id id3);;
assert (size_after opt_insns opt_cam id3   <= size_after id id id3);;
assert (size_after id        opt_cam add23 <= size_after id id add23);;
assert (size_after opt_insns opt_cam add23 <= size_after id id add23);;
