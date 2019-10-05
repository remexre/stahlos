open Forth
open Utils

type value
  = Code of string option * int
  | EmptyEnv
  | Lit of int
  | Pair of value * value
  | Stop

let rec string_of_value : value -> string = function
  | Code(Some(name), addr) -> "Code(Some(" ^ name ^ "), " ^ string_of_int addr ^ ")"
  | Code(None, addr) -> "Code(None, " ^ string_of_int addr ^ ")"
  | EmptyEnv -> "EmptyEnv"
  | Lit(n) -> "Lit(" ^ string_of_int n ^ ")"
  | Pair(l, r) -> "Pair(" ^ string_of_value l ^ ", " ^ string_of_value r ^ ")"
  | Stop -> "Stop"

type t =
  { program : program
  ; data_stack : value list
  ; return_stack : value list
  ; program_counter : value
  }

let from_program (program: program) : t =
  { program = program
  ; data_stack = [EmptyEnv]
  ; return_stack = [Stop]
  ; program_counter = Code(None, 0)
  }

let to_string (state: t) : string =
  "{ program = " ^ string_of_program state.program ^ "\n" ^
  "; data_stack = [" ^ Utils.join_with ", " (List.map string_of_value state.data_stack) ^ "]\n" ^
  "; return_stack = [" ^ Utils.join_with ", " (List.map string_of_value state.return_stack) ^ "]\n" ^
  "; program_counter = " ^ string_of_value state.program_counter ^ "\n" ^
  "}"

exception Illegal_program_counter of value
exception Multiple_values of value list
exception Return_stack_underflow
exception Stack_underflow
exception Type_error of value * string

let step (state: t) : t =
  let (dhd, dtl) = match state.data_stack with
  | [] -> raise Stack_underflow
  | h::t -> (h, t)
  and ds = state.data_stack
  and rs = state.return_stack
  and pc = match state.program_counter with
  | Code(n, a) -> (n, a)
  | value -> raise (Illegal_program_counter(value))
  in
  let next_word = match fst pc with
  | Some(n) -> List.nth_opt (List.assoc n state.program.defs) (snd pc)
  | None -> List.nth_opt state.program.main (snd pc)
  and pc' = ref (Code(fst pc, snd pc + 1))
  and rs' = ref rs
  in
  let ds' = match next_word with
  | Some(App) -> (match dhd with
                 | Pair(Pair(code, env), arg) ->
                     rs' := !pc' :: !rs';
                     pc' := code;
                     Pair(env, arg)::dtl
                 | _ -> raise (Type_error(dhd, "app")))
  | Some(Lam(b)) -> Pair(Code(Some(b), 0), dhd)::dtl
  | Some(Fst) -> (match dhd with
                 | Pair(l, _) -> l::dtl
                 | _ -> raise (Type_error(dhd, "fst")))
  | Some(Snd) -> (match dhd with
                 | Pair(_, r) -> r::dtl
                 | _ -> raise (Type_error(dhd, "snd")))
  | Some(Cons) -> (match dtl with
                  | (cadr::cddr) -> Pair(cadr, dhd)::cddr
                  | [] -> raise Stack_underflow)
  | Some(Dup) -> dhd::ds
  | Some(Swap) -> (match dtl with
                  | (cadr::cddr) -> cadr::dhd::cddr
                  | [] -> raise Stack_underflow)
  | Some(QuoteName(n)) -> Code(Some(n), 0)::dtl
  | Some(QuoteNum(n)) -> Lit(n)::dtl
  | Some(w) -> print_endline (to_string state);
               failwith ("TODO Forth_vm.step [" ^ Forth.string_of_word w ^ "]")
  | None -> (match rs with
            | [] -> raise Return_stack_underflow
            | rhd::rtl -> rs' := rtl; pc' := rhd; ds)
  in { program = state.program; data_stack = ds'
     ; return_stack = !rs'; program_counter = !pc' }

let rec run_exn (state: t) : value =
  if state.program_counter = Stop then
    match state.data_stack with
    | [v] -> v
    | vs -> raise (Multiple_values(vs))
  else
    run_exn (step state)

type exec_error
  = IllegalProgramCounter of value
  | MultipleValues of value list
  | ReturnStackUnderflow
  | StackUnderflow
  | TypeError of value * string

let string_of_exec_error : exec_error -> string = function
  | IllegalProgramCounter(v) -> "IllegalProgramCounter(" ^ string_of_value v ^ ")"
  | MultipleValues(vs) -> "MultipleValues([" ^ join_with ", " (List.map string_of_value vs) ^ "])"
  | ReturnStackUnderflow -> "ReturnStackUnderflow"
  | StackUnderflow -> "StackUnderflow"
  | TypeError(v, s) -> "TypeError(" ^ string_of_value v ^ ", " ^ s ^ ")"

let run (state: t) : (value, exec_error) result =
  try Ok(run_exn state)
  with Illegal_program_counter(v) -> Error(IllegalProgramCounter(v))
     | Multiple_values(vs) -> Error(MultipleValues(vs))
     | Return_stack_underflow -> Error(ReturnStackUnderflow)
     | Stack_underflow -> Error(StackUnderflow)
     | Type_error(v, s) -> Error(TypeError(v, s))
