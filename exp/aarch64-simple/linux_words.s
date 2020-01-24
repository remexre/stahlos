.include "words.inc"

.section .text

.global linux_exit
.global linux_exit.cfa
linux_exit:
	.quad linux_pog
	.byte 0
	.byte 10
	.ascii "LINUX/EXIT"
	.align 4, 0
linux_exit.cfa:
	mov x0, x10
	mov x8, #93
	svc #0

.global linux_pog
.global linux_pog.cfa
linux_pog:
	.quad literal /* TODO: Update to first word of builtins */
	.byte 0
	.byte 9
	.ascii "LINUX/POG"
	.align 4, 0
linux_pog.cfa:
	mov x0, #1
	ldr x1, =linux_pog.msg
	ldr x2, =linux_pog.len
	mov x8, #64
	svc #0
	NEXT
linux_pog.msg: .ascii "\x1b[31mP\x1b[32mO\x1b[33mG\x1b[34mG\x1b[35mE\x1b[36mR\x1b[37mS\x1b[0m\n"
linux_pog.len = . - linux_pog.msg

/* vi: set ft=arm64asm : */
