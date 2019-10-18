exception Invalid_defined_name of string * Ast.def
exception Invalid_name of string
exception Redefined_name of string * Ast.def
exception Unbound_name of string * Ast.expr list

val for_module : Ast.module_ -> unit
