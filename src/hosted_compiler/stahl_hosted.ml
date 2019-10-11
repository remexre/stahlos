open Utils

let () =
  try
    let m = Ast.load_module_from stdin in
    Check_names.for_module m;
    print_endline (Ast.string_of_module m);
    let m = Tyck.tyck_module m in
    print_endline "---";
    print_endline (Tast.string_of_module m);
    ()
  with
    Ast.Invalid_ast(msg, sexpr) ->
      prerr_string "Invalid AST: ";
      prerr_endline msg;
      prerr_endline (Sexpr.to_string sexpr);
      exit 2

let () =
  print_newline ();
  print_string (read_file_string "src/hosted_compiler/runtime.fth")

let () =
  let _ = Cam.optimize in
  ()
