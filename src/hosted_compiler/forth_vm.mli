type value
  = Code of string option * int
  | EmptyEnv
  | Lit of int
  | Pair of value * value
  | Stop

val string_of_value : value -> string

type t

val from_program : Forth.program -> t

exception Illegal_program_counter of value
exception Multiple_values of value list
exception Return_stack_underflow
exception Stack_underflow
exception Type_error of value * string

val step : t -> t

val run_exn : t -> value

type exec_error
  = IllegalProgramCounter of value
  | MultipleValues of value list
  | ReturnStackUnderflow
  | StackUnderflow
  | TypeError of value * string

val string_of_exec_error : exec_error -> string

val run : t -> (value, exec_error) result
