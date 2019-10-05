open Utils

let () =
  let src = read_all_string stdin in
  prerr_endline (String.escaped src);
  let sexprs = must id (Sexpr.parse src) in
  List.iter (print_endline %% Sexpr.to_string) sexprs

let () =
  print_endline ".( Hello, Stahl world!)";
  print_string "\\ ";
  print_endline (Sexpr.to_string (Sexpr.String("foo\nbar")))

let () =
  print_newline ();
  print_string (read_file_string "src/hosted_compiler/runtime.fth")
