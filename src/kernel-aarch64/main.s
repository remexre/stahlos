.section .text

.global _start
_start:
	ldr x0, =qemuvirt_uart_init
	ldr x1, =qemuvirt_uart_read_line
	ldr x2, =qemuvirt_uart_write_string
	bl format_init

_start.loop:
	ldr x0, =prompt
	mov x1, prompt.len
	bl format_write_string

	ldr x0, =buf
	mov x1, 256
	bl format_read_line

	mov x10, x1

	ldr x0, =buf
	bl format_write_string
	bl format_write_newline

	mov x0, x10
	bl format_write_num_ux

	b _start.loop

.section .rodata

msg: .string "Hello, world!\r\n"
.equ len, . - msg

prompt: .string "\r\n> "
.equ prompt.len, . - prompt

.section .bss

.comm buf, 256

/* vi: set ft=arm64asm : */
