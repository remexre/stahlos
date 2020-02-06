.section .text

.macro defword label, name, flags=0
.global \label
\label\().padding:
.fill (258 - (\label - \label\().name)) % 4
\label\().header:
	.long last_defword
	.byte \flags
	.byte \label - \label\().name
\label\().name:
	.ascii "\name"
\label:
.set last_defword, \label\().header
.endm
.set last_defword, 0

.macro pop
	cbz x12, panic.underflow_data_stack
	sub x12, x12, 1
	ldr x10, [x11, x12]
.endm

.macro push
	cmp x12, x13
	b.ge panic.out_of_data_stack
	str x10, [x11, x12]
	add x12, x12, 1
.endm

.macro next
	ldr x0, [x18], #8
	br x0
.endm

defword forth_dot, "."
	mov x0, x10
	pop
	bl format_write_num_ux
	next

defword forth_literal_impl, "(LITERAL)"
	push
	ldr x10, [x18], #8
	next

defword forth_panic, "PANIC"
	b panic.forth

/* vi: set ft=arm64asm : */
