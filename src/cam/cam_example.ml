(*********)
(* utils *)
(*********)

let id (x: 'a) : 'a = x

(****************************)
(* Our lambda calculus term *)
(****************************)

type lam
  = Abs of lam
  | App of lam * lam
  | Con of string
  | Var of int

let kiuv = App(App(App(Abs(Abs(Var(1))), Abs(Var(0))), Con("u")), Con("v"))
let kuv = App(App(Abs(Abs(Var(1))), Con("u")), Con("v"))
let idw = App(Abs(App(Abs(Var(0)), Var(0))), Con("w"))

(**********************)
(* Static combinators *)
(**********************)

type cam
  = SApp
  | SFst
  | SSnd
  | SQuote of string
  | SLam of cam
  | SCom of cam * cam
  | SPair of cam * cam
  | SId (* only used later on, in optimization *)

let rec cam_of_lam : lam -> cam = function
  | Abs(b) -> SLam(cam_of_lam b)
  | App(f, x) -> SCom(SApp, SPair(cam_of_lam f, cam_of_lam x))
  | Con(s) -> SQuote(s)
  | Var(n) -> if n < 0 then
                failwith "Invalid de Brujin index"
              else if n = 0 then
                SSnd
              else
                SCom(cam_of_lam(Var(n-1)), SFst)

let static_kiuv =
  let app x y = SCom(SApp, SPair(x, y)) in
  app (app (app (SLam(SLam(SCom(SSnd, SFst)))) (SLam(SSnd))) (SQuote("u"))) (SQuote("v"))

let static_kuv =
  let app x y = SCom(SApp, SPair(x, y)) in
  app (app (SLam(SLam(SCom(SSnd, SFst)))) (SQuote("u"))) (SQuote("v"))

let static_idw =
  let app x y = SCom(SApp, SPair(x, y)) in
  app (SLam(app (SLam(SSnd)) SSnd)) (SQuote("w"))

;;
(* wtf ocaml... the ;; above seems to be necessary... *)

assert (cam_of_lam kiuv = static_kiuv);;
assert (cam_of_lam kuv = static_kuv);;
assert (cam_of_lam idw = static_idw)

(********************************)
(* Static + dynamic combinators *)
(********************************)

type cam_runtime
  = CApp
  | CFst
  | CSnd
  | CQuote of string
  | CLam of cam_runtime
  | CCom of cam_runtime * cam_runtime
  | CPair of cam_runtime * cam_runtime
  | CId
  (* Dynamic things below *)
  | CEmpty
  | CCon of string
  | CApply of cam_runtime * cam_runtime
  | CDPair of cam_runtime * cam_runtime

let rec inj : cam -> cam_runtime = function
  | SApp -> CApp
  | SFst -> CFst
  | SSnd -> CSnd
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
  (* apply them recursively *)
  | CApp -> CApp
  | CFst -> CFst
  | CSnd -> CSnd
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

assert (eval_cam (cam_of_lam kiuv) = CCon("v"));;
assert (eval_cam (cam_of_lam kuv) = CCon("u"));;
assert (eval_cam (cam_of_lam idw) = CCon("w"))

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
  | IQuote of string
  | ISwap

let rec insns_of_cam : cam -> insn list = function
  | SApp -> [IApp]
  | SFst -> [IFst]
  | SSnd -> [ISnd]
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
  | ICon of string
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

assert (eval_insns (insns_of_cam (cam_of_lam kiuv)) = ICon("v"));;
assert (eval_insns (insns_of_cam (cam_of_lam kuv)) = ICon("u"));;
assert (eval_insns (insns_of_cam (cam_of_lam idw)) = ICon("w"))

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

assert (eval_cam (opt_cam (cam_of_lam kiuv)) = CCon("v"));;
assert (eval_cam (opt_cam (cam_of_lam kuv)) = CCon("u"));;
assert (eval_cam (opt_cam (cam_of_lam idw)) = CCon("w"))

let size_after f g expr = List.length (f (insns_of_cam (g (cam_of_lam expr))));;

assert (size_after id        opt_cam kiuv < size_after id id kiuv);;
assert (size_after opt_insns opt_cam kiuv < size_after id id kiuv);;
assert (size_after id        opt_cam kuv  < size_after id id kuv);;
assert (size_after opt_insns opt_cam kuv  < size_after id id kuv);;
assert (size_after id        opt_cam idw  < size_after id id idw);;
assert (size_after opt_insns opt_cam idw  < size_after id id idw);;
