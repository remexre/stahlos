(*
open Utils

let ki12 =
  let open Lam in
  App(App(App(Lam(Lam(Var(1))), Lam(Var(0))), LitNum(1)), LitNum(2))

type ('a, 'b) either = Left of 'a | Right of 'b

let run' =
     Forth_vm.string_of_value %% must Forth_vm.string_of_exec_error %% Forth_vm.run
  %% Forth_vm.from_program

let () =
  print_endline (Lam.to_string ki12);
  print_endline (Cam.to_string (Lam.compile_to_cam ki12));
  let forth = Lam.compile_to_forth ki12 in
  print_endline (Forth.string_of_program forth);
  print_endline (run' forth)

let () =
  print_endline (Sexpr.to_string (Sexpr.Symbol("test")));
  let sexprs = must id (Sexpr.parse " one '(2 () three) 4 ") in
  List.iter (print_endline %% Sexpr.to_string) sexprs

let () =
  print_endline (read_file_string "src/hosted_compiler/cam.ml")

let () = print_endline "Done"
*)

let () =
  print_endline ".( Hello, Stahl world!)"
