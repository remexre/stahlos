.section .text

.global _start
_start:
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
	b _start

write:
	ldr w2, [x1,0x18]
	tbz w2, 5, write.do
write.do:
	str w0, [x1]
	br lr

/* vi: set ft=arm64asm : */
