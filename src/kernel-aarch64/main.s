.section .text

.global _start
_start:
	ldr x0, =qemuvirt_uart_init
	ldr x1, =qemuvirt_uart_read_line
	ldr x2, =qemuvirt_uart_write_string
	bl format_init

	/* Set up process table pointer, init process table */
	ldr x19, =init_proctbl
	ldr x0, =forth.last_pseudobuiltin_header
	str x0, [x19]
	/* Source gets set up by init */

	/* Set up data stack */
	mov x10, xzr
	add x11, x19, #0x200
	mov x12, xzr
	mov x13, #0x200

	/* Set up return stack */
	mov x14, xzr
	add x15, x19, #0x80
	mov x16, xzr
	mov x17, #0x180

	/* Set up instruction pointer, and run NEXT */
	ldr x18, =init
	ldr x0, [x18], #8
	br x0

.section .rodata

init:
	.quad forth_impl_literal, init.start
	.quad forth_impl_literal, (init.end - init.start)
	.quad forth_false
	.quad forth_evaluate
	.quad forth_panic

init.start:
.incbin "init.fth"
init.end:

.section .bss

.comm init_proctbl, 1024, 8

/* vi: set ft=arm64asm : */
