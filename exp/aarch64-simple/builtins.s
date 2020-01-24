.include "words.inc"

.section .text

.global literal
.global literal.cfa
literal:
	.quad load
	.byte 0
	.byte 7
	.ascii "LITERAL"
	.align 4, 0
literal.cfa:
	add x11, x11, 8
	str x10, [x11]
	ldr x10, [x18]
	add x18, x18, 8
	NEXT

.global load
.global load.cfa
load:
	.quad 0
	.byte 0
	.byte 1
	.ascii "@"
	.align 4, 0
load.cfa:
	ldr x10, [x10]
	NEXT

/* vi: set ft=arm64asm : */
