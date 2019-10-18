open Utils

let () =
  try
    let m = Ast.load_module_from stdin in
    Check_names.for_module m;
    print_endline (Ast.string_of_module m);
    let m = Nast.resolve_names_for_module m in
    let _ = m in
    (*
    let m = Tyck.tyck_module m in
    print_endline "---";
    print_endline (Tast.string_of_module m);
    *)
    ()
  with
    | Ast.Invalid_ast(msg, sexpr) ->
        prerr_string "Invalid AST: ";
        prerr_endline msg;
        prerr_endline (Sexpr.to_string sexpr);
        exit 2
    | Nast.Unbound_name(name, es) ->
        prerr_string "Unbound name: ";
        prerr_endline name;
        List.iter
          (fun e ->
            prerr_string "In expression ";
            prerr_endline (Ast.string_of_expr e))
          es;
        exit 3
    (*
    | Tyck_ctx.Failed_to_solve(cstr) ->
        prerr_string "Failed to solve constraint: ";
        prerr_endline (Tyck_ctx.string_of_cstr cstr);
        exit 4
    | Tyck_ctx.Unsolved_variable(n, es) ->
        List.iter
          (fun e ->
            prerr_string "While walking ";
            prerr_endline (Uast.string_of_expr_inner e))
          es;
        prerr_string "Unsolved variable: ?%";
        prerr_int n;
        prerr_newline ();
        exit 5
    *)

let () =
  print_newline ();
  print_string (read_file_string "src/hosted_compiler/runtime.fth")

let () =
  let _ = Cam.optimize in
  ()
