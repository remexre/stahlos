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
	b \cfa
\label\().pfa:
.set last_defword, \label\().header
.endm

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

	.quad forth_dot_hex
	.quad forth_cr

	.quad forth_impl_branch, forth_evaluate.loop

forth_evaluate.end:
	.quad forth_from_rstack
	.quad forth_from_rstack
	.quad forth_from_rstack
	.quad forth_set_source
	.quad forth_impl_semicolon

defword forth_parse_name, "PARSE-NAME"
	.quad forth_skip_spaces
	.quad forth_impl_literal, 0x20
	.quad forth_parse
	.quad forth_impl_semicolon

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

/* vi: set ft=arm64asm : */
