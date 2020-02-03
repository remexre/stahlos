.section .text

.global _start
_start:
	bl format_init

	ldr x0, =msg
	mov x1, len
	bl format_write_string

_start.loop:
	ldr x0, =prompt
	mov x1, prompt.len
	bl format_write_string

	ldr x0, =buf
	mov x1, 256
	bl format_read_line

	ldr x0, =buf
	bl format_write_string
	bl format_write_newline

	b _start.loop

.section .rodata

msg: .string "Hello, world!\r\n"
.equ len, . - msg

prompt: .string "\r\n> "
.equ prompt.len, . - prompt

.section .bss

.comm buf, 256

/* vi: set ft=arm64asm : */
