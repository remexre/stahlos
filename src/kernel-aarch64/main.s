.section .text

.global _start
_start:
	ldr x1, =0xff1a0000
	mov x0, 'x'
	str x0, [x1]
	b _start

/* vi: set ft=arm64asm : */
