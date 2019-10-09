open Utils

let () =
  try
    let m = Ast.load_module_from stdin in
    print_endline (Ast.string_of_module m)
  with
    Ast.Invalid_ast(msg, sexpr) ->
      prerr_string "Invalid AST: ";
      prerr_endline msg;
      prerr_endline (Sexpr.to_string sexpr);
      exit 2

let () =
  print_endline ".( Hello, Stahl world!)";
  print_string "\\ ";
  print_endline (Sexpr.to_string (Sexpr.String("foo\nbar")))

let () =
  print_newline ();
  print_string (read_file_string "src/hosted_compiler/runtime.fth")
