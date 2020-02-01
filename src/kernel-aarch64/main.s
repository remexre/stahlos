.section .text

.global _start
_start:
	bl uart_init

loop:
	ldr x1, =0xff1a0000
	mov x0, 'o'
	bl write
	mov x0, 's'
	bl write
	mov x0, 'u'
	bl write
	mov x0, '!'
	bl write
	mov x0, ' '
	bl write
	mov x0, '-'
	bl write
	mov x0, '-'
	bl write
	mov x0, ' '
	bl write
	mov x0, 'S'
	bl write
	mov x0, 't'
	bl write
	mov x0, 'a'
	bl write
	mov x0, 'h'
	bl write
	mov x0, 'l'
	bl write
	mov x0, #10
	bl write

	mov x0, xzr
	mov x1, xzr
	bl uart_get_line

	b loop

write:
	ldr w2, [x1,0x18]
	tbz w2, 5, write.do
write.do:
	str w0, [x1]
	ret

/* vi: set ft=arm64asm : */
