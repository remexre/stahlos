.macro NEXT
	ldr x0, [x18]
	add x18, x18, 8
	br x0
.endm

/* vi: set ft=arm64asm : */
