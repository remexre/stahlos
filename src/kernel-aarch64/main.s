.section .text

.global _start
_start:
	bl uart_init

loop:
	/*ldr x0, =osu*/
	/*mov x1, osu.len*/
	/*bl uart_write*/

	ldr x0, =0xff1a0000
	ldrb w1, [x0, #0x14]
	/*mov x1, '!'*/
	strb w1, [x0]

	b loop

.section .rodata

osu: .string "osu! -- StahlOS\n"
.equ osu.len, . - osu

/* vi: set ft=arm64asm : */
