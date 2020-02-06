.section .text

.global _start
_start:
	ldr x0, =qemuvirt_uart_init
	ldr x1, =qemuvirt_uart_read_line
	ldr x2, =qemuvirt_uart_write_string
	bl format_init

	ldr x25, =init_proctbl

	mov x10, xzr
	add x11, x25, #0x200
	mov x12, xzr
	mov x13, #0x200

	mov x14, xzr
	add x15, x25, #0x80
	mov x16, xzr
	mov x17, #0x180

	ldr x18, =preinit

	ldr x0, [x18], #8
	br x0

.section .rodata

preinit:
	.quad forth_literal_impl
	.quad 42
	.quad forth_dot
	.quad forth_panic

.section .bss

.comm init_proctbl, 1024, 8

/* vi: set ft=arm64asm : */
