.section .text

.set last_defword, forth.last_builtin_header
.macro defword label, name, flags=0, cfa=forth_impl_colon
.global \label
\label\().padding:
.fill (258 - (\label - \label\().name)) % 4
\label\().header:
	.quad last_defword
	.byte \flags
	.byte \label - \label\().name
\label\().name:
	.ascii "\name"
\label:
	ldr x1, .+8
	br x1
	.quad \cfa
\label\().pfa:
.set last_defword, \label\().header
.endm

defword forth_create, "CREATE"
	.quad forth_parse_name
	.quad forth_dup
	.quad forth_impl_branch_zero, forth_create.error
	.quad forth_dup
	.quad forth_impl_literal, 3
	.quad forth_and
	.quad forth_impl_literal, 2
	.quad forth_swap
	.quad forth_minus
	.quad forth_impl_literal, 3
	.quad forth_and
	.quad forth_allot

	.quad forth_process_table
	.quad forth_load_qword
	.quad forth_here
	.quad forth_process_table
	.quad forth_store_qword
	.quad forth_comma
	.quad forth_impl_literal, 0
	.quad forth_comma_char

	.quad forth_dup
	.quad forth_comma_char

forth_create.write_name:
	.quad forth_dup
	.quad forth_impl_branch_zero, forth_create.write_cfa
	.quad forth_swap
	.quad forth_dup
	.quad forth_load_char
	.quad forth_comma_char
	.quad forth_one_plus
	.quad forth_swap
	.quad forth_one_minus
	.quad forth_impl_branch, forth_create.write_name

forth_create.write_cfa:
	.quad forth_two_drop
	.quad forth_impl_literal, 16
	.quad forth_allot
	.quad forth_impl_literal, forth_impl_variable
	.quad forth_set_does
	.quad forth_impl_semicolon
forth_create.error:
	.quad panic.end_of_source_when_parsing

defword forth_evaluate, "EVALUATE"
	.quad forth_source
	.quad forth_to_rstack
	.quad forth_to_rstack
	.quad forth_to_rstack
	.quad forth_set_source

forth_evaluate.loop:
	.quad forth_parse_name
	.quad forth_dup

	.quad forth_impl_branch_zero, forth_evaluate.end

	.quad forth_two_dup
	.quad forth_find_header

	.quad forth_dup
	.quad forth_impl_branch_zero, forth_evaluate.loop.try_as_number

	.quad forth_to_rstack
	.quad forth_two_drop
	.quad forth_from_rstack
	.quad forth_dup
	.quad forth_header_to_xt
	.quad forth_swap
	.quad forth_header_to_immediate
	.quad forth_flags
	.quad forth_impl_literal, 1
	.quad forth_and
	.quad forth_or

	.quad forth_impl_branch_zero, forth_evaluate.loop.compile_word
	.quad forth_execute
	.quad forth_impl_branch, forth_evaluate.loop
forth_evaluate.loop.compile_word:
	.quad forth_compile_comma
	.quad forth_impl_branch, forth_evaluate.loop

forth_evaluate.loop.try_as_number:
	.quad forth_drop
	.quad forth_two_dup
	.quad forth_to_number
	.quad forth_impl_branch_zero, forth_evaluate.loop.number
	.quad forth_drop
	.quad forth_impl_literal, word_not_found
	.quad forth_impl_literal, word_not_found.len
	.quad forth_type
	.quad forth_type
	.quad forth_cr
	.quad panic.word_not_found

forth_evaluate.loop.number:
	.quad forth_flags
	.quad forth_impl_literal, 1
	.quad forth_and
	.quad forth_impl_branch_zero, forth_evaluate.loop.number.compile

forth_evaluate.loop.number.interpret:
	.quad forth_rot_rev
	.quad forth_two_drop
	.quad forth_impl_branch, forth_evaluate.loop

forth_evaluate.loop.number.compile:
	.quad forth_impl_literal, forth_impl_literal
	.quad forth_compile_comma
	.quad forth_comma
	.quad forth_two_drop
	.quad forth_impl_branch, forth_evaluate.loop

forth_evaluate.end:
	.quad forth_two_drop
	.quad forth_from_rstack
	.quad forth_from_rstack
	.quad forth_from_rstack
	.quad forth_set_source
	.quad forth_impl_semicolon

defword forth_header_to_immediate, "HEADER>IMMEDIATE?"
	.quad forth_header_to_flags
	.quad forth_impl_literal, 1
	.quad forth_and
	.quad forth_boolify
	.quad forth_impl_semicolon

defword forth_quote, "'"
	.quad forth_parse_name
	.quad forth_dup
	.quad forth_impl_branch_zero, forth_quote.error
	.quad forth_find_header
	.quad forth_header_to_xt
	.quad forth_impl_semicolon
forth_quote.error:
	.quad panic.end_of_source_when_parsing

defword forth_two_drop, "2DROP"
	.quad forth_drop
	.quad forth_drop
	.quad forth_impl_semicolon

defword forth_two_dup, "2DUP"
	.quad forth_over
	.quad forth_over
	.quad forth_impl_semicolon

defword forth_type, "TYPE"
forth_type.loop:
	.quad forth_dup
	.quad forth_impl_branch_zero, forth_type.end
	.quad forth_swap
	.quad forth_dup
	.quad forth_load_char
	.quad forth_emit
	.quad forth_one_plus
	.quad forth_swap
	.quad forth_one_minus
	.quad forth_impl_branch, forth_type.loop
forth_type.end:
	.quad forth_two_drop
	.quad forth_impl_semicolon

.global forth.last_pseudobuiltin_header
.equiv forth.last_pseudobuiltin_header, last_defword

.section .rodata

word_not_found: .ascii "Word not found: "
.equ word_not_found.len, . - word_not_found

/* vi: set ft=arm64asm : */
