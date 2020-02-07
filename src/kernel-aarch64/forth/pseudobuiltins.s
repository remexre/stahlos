.section .text

.macro defword label, name, flags=0, cfa=forth_impl_colon
.global \label
\label\().padding:
.fill (258 - (\label - \label\().name)) % 4
\label\().header:
	.long forth_builtins_last_defword
	.byte \flags
	.byte \label - \label\().name
\label\().name:
	.ascii "\name"
\label:
	b \cfa
\label\().pfa:
.set forth_builtins_last_defword, \label\().header
.endm

defword forth_evaluate, "EVALUATE"
	.quad forth_source
	.quad forth_dot_hex
	.quad forth_cr
	.quad forth_dot_hex
	.quad forth_cr
	.quad forth_panic

defword forth_foo, "FOO"
	.quad forth_dup
	.quad forth_one_plus
	.quad forth_dot_hex
	.quad forth_cr
	.quad forth_dot_hex
	.quad forth_cr
	.quad forth_impl_semicolon

/* vi: set ft=arm64asm : */
