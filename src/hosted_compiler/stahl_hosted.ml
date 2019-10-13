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
    | Ast.Invalid_ast(msg, sexpr) ->
        prerr_string "Invalid AST: ";
        prerr_endline msg;
        prerr_endline (Sexpr.to_string sexpr);
        exit 2
    | Tyck.Failed_to_solve(cstr) ->
        prerr_string "Failed to solve constraint: ";
        prerr_endline (Tyck.string_of_cstr cstr);
        exit 3
    | Tyck.Unsolved_variable(n) ->
        prerr_string "Unsolved variable: ?%";
        prerr_int n;
        prerr_newline ();
        exit 4

let () =
  print_newline ();
  print_string (read_file_string "src/hosted_compiler/runtime.fth")

let () =
  let _ = Cam.optimize in
  ()
