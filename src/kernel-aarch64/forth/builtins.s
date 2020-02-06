.section .text

.macro pop
	cbz x12, panic.underflow_data_stack
	sub x12, x12, 1
	ldr x10, [x11, x12]
.endm

.macro push
	add x12, x12, 1
	cmp x12, x13
	b.ge panic.out_of_data_stack
	str x10, [x11, x12]
.endm

.macro next
	ldr x0, [x18], #8
	br x0
.endm



.global forth_dot
forth_dot.header:
	.long 0
	.byte 1
	.string "."
forth_dot:
	mov x0, x10
	pop
	bl format_write_num_ux
	next



.global forth_literal_impl
forth_literal_impl.header:
	.long forth_dot.header
	.byte 9
	.string "(LITERAL)"
forth_literal_impl:
	push
	ldr x10, [x18], #8
	next



.global forth_panic
forth_panic.header:
	.long forth_literal_impl.header
	.byte 5
	.string "PANIC"
forth_panic:
	b panic.forth



/* vi: set ft=arm64asm : */
