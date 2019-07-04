Forth Words
===========

Builtins
--------

```
defcode abs, "ABS", 1
defcode add, "+", 2
defcode add_store, "+!", 2
defcode adjust_string, "/STRING", 3
defcode allot, "ALLOT", 1
defcode and, "AND", 2
defcode arith_right_shift, "ARSHIFT", 2
defcode base_decimal, "DECIMAL"
defcode empty_return_stack, "EMPTY-RETURN-STACK"
defcode base_hex, "HEX"
defcode cell, "CELLS", 1
defcode cell_plus, "CELL+", 1
defcode decr, "1-", 1
defcode depth, "DEPTH"
defcode docolon, "((DOCOLON))"
defcode dodoes, "((DODOES))"
defcode dovar, "((DOVAR))"
defcode drop, "DROP", 1
defcode dup, "DUP", 1
defcode dup_nonzero, "?DUP", 1
defcode equal, "=", 2
defcode execute, "EXECUTE", 1
defcode exit, "EXIT"
defcode false, "FALSE"
defcode fetch, "@", 1
defcode fetch_char, "C@", 1
defcode fetch_dword, "D@", 1
defcode fetch_word, "W@", 1
defcode from_r, "R>"
defcode from_r_2, "2R>"
defcode get_base, "GET-BASE"
defcode get_state, "GET-STATE"
defcode here, "HERE"
defcode hlt, "HLT"
defcode if_impl, "(IF)", 1
defcode inb, "INB", 1
defcode incr, "1+", 1
defcode int3, "INT3"
defcode invert, "INVERT", 1
defcode jump_impl, "(JUMP)"
defcode left_shift, "LSHIFT", 2
defcode literal_impl, "(LITERAL)"
defcode literal_r_impl, "(LITERAL-R)"
defcode mul_d, "*D", 2
defcode mul_div_mod, "*/MOD", 3
defcode n_to_str, "N>STR", 1
defcode negate, "NEGATE", 1
defcode noop, "NOOP"
defcode or, "OR", 2
defcode outb, "OUTB", 2
defcode outw, "OUTW", 2
defcode pick, "PICK", 1
defcode r_fetch, "R@"
defcode rev_rot, "-ROT", 3
defcode right_shift, "RSHIFT", 2
defcode rot, "ROT", 3
defcode rpick, "RPICK", 1
defcode rpick_addr, "RPICK-ADDR", 1
defcode s_quote_impl, '(S")',
defcode s_to_d, "S>D", 1
defcode source_buffer, "(SOURCE-BUFFER)"
defcode source_length, "(SOURCE-LENGTH)"
defcode state_compile, "]"
defcode state_interpret, "[", 0, 0x01
defcode store, "!", 2
defcode store_char, "C!", 2
defcode store_dword, "D!", 2
defcode store_word, "W!", 2
defcode streq, "STRING=", 4
defcode sub, "-", 2
defcode swap, "SWAP", 2
defcode test_flag, "TEST-FLAG", 2
defcode to_in, ">IN"
defcode to_number, ">NUMBER", 2
defcode to_r, ">R", 1
defcode to_r_2, "2>R", 2
defcode true, "TRUE"
defcode u_greater, "U>", 2
defcode u_less, "U<", 2
defcode user_pointer, "USER-POINTER"
defcode zero_equal, "0="
defcode last_builtin, "", 0
```

Pseudobuiltins
--------------

```
defcolon comma, ","
defcolon comma_char, "C,"
defcolon comma_dword, "D,"
defcolon comma_str, "S,"
defcolon comma_word, "W,"
defcolon comment, "\", 0x01
defcolon compile_comma, "COMPILE,"
defcolon count, "COUNT"
defcolon create, "CREATE"
defcolon create_noname, "CREATE-NONAME"
defcolon dict_head, "DICT-HEAD"
defcolon does_enter, "DOES>ENTER"
defcolon drop2, "2DROP"
defcolon dup2, "2DUP"
defcolon evaluate, "EVALUATE"
defcolon find_header, "FIND-HEADER"
defcolon header_to_cfa, "HEADER>CFA"
defcolon header_to_name, "HEADER>NAME"
defcolon immediate, "IMMEDIATE"
defcolon interpret, "INTERPRET"
defcolon is_immediate, "IMMEDIATE?"
defcolon is_nl, "IS-NL?"
defcolon is_space, "IS-SPACE?"
defcolon isnt_space, "ISNT-SPACE?"
defcolon latest, "LATEST"
defcolon not_equal, "<>"
defcolon over, "OVER"
defcolon parse_name, "PARSE-NAME"
defcolon source, "SOURCE"
defcolon source_rest, "SOURCE-REST"
defcolon source_skip_spaces, "SOURCE-SKIP-SPACES"
defcolon string_find_pred, "STRING-FIND-PRED"
```
