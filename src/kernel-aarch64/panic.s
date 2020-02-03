.section .text

.global panic
panic:
	ldr x0, =msg
	mov x1, len
	ldr x30, =panic
	b format_write_string

.section .rodata

msg: .string "panic!\r\n"
.equ len, . - msg

/* vi: set ft=arm64asm : */
