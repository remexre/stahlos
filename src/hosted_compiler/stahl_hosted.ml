open Utils

let usage = "usage"

let main' (input_path: string) (output_path: string option) (verbose: bool) : unit =
  Utils.log_set_enabled verbose;
  with_output output_path (fun out ->
    begin
      try
        let m = Ast.load_module_from_path input_path in
        Utils.logln "---\nAst:";
        Utils.logln (Ast.string_of_module m);
        Check_names.for_module m;
        let m = Nast.resolve_names_for_module m in
        Utils.logln "---\nNast:";
        Utils.logln (Nast.string_of_module m);
        let _ = m in
        (*
        let m = Tyck.tyck_module m in
        print_endline "---";
        print_endline (Tast.string_of_module m);
        *)
        output_string out "\n";
        output_string out (read_file_string "src/hosted_compiler/runtime.fth")
      with
        | Ast.Invalid_ast(msg, sexpr) ->
            prerr_string "Invalid AST: ";
            prerr_endline msg;
            prerr_endline (Sexpr.to_string sexpr);
            exit 2
        | Check_names.Unbound_name(name, es) ->
            prerr_string "Unbound name: ";
            prerr_endline name;
            List.iter
              (fun e ->
                prerr_string "In expression ";
                prerr_endline (Ast.string_of_expr e))
              (List.rev es);
            exit 3
    end)

let main argv =
  let opt_input_path = ref None in
  let opt_output_path = ref None in
  let opt_verbose = ref false in
  let args =
    [ ("-o", Arg.String (fun s -> opt_output_path := Some(s)), "Sets the output path.")
    ; ("-v", Arg.Set opt_verbose, "Enables logging.")
    ] in
  Arg.parse_argv argv args (fun s ->
    begin
      match !opt_input_path with
      | Some(_) ->
        prerr_endline "Extra required argument";
        Arg.usage args usage;
        exit 1
      | None -> ()
    end;
    opt_input_path := Some(s))
    usage;
  let ensure s opt =
    match opt with
    | Some(x) -> x
    | None ->
        prerr_endline ("Missing " ^ s ^ ".");
        Arg.usage args usage;
        exit 1
  in
  main' (ensure "input path" !opt_input_path) !opt_output_path !opt_verbose

let () = main Sys.argv

(* ensures Cam still compiles *)
let () = let _ = Cam.optimize in ()
