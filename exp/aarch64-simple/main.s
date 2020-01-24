.section .text

.global _start
_start:
	/* Initialize stuff needed for I/O */
	bl init_epoll
	brk #0

	/* Init parameter stack */
	ldr x11, =ps
	mov x12, #0

	/* Init return stack */
	ldr x14, =ps
	mov x15, #0

	/* Init Forth instruction pointer */
	ldr x18, =init

	/* TODO: Process Information Pointer */

	/* NEXT */
	ldr x0, [x18]
	add x18, x18, 8
	br x0

.section .rodata

init:
	/* .quad literal.cfa */
	/* .quad 1337 */
	/* TODO: D>S */
	/* .quad dot_s.cfa */
	/* .quad literal.cfa */
	/* .quad 42 */
	.quad linux_pog.cfa
	.quad linux_exit.cfa

.section .bss

.global ps /* DEBUG */
.lcomm ps, 4096
.global rs /* DEBUG */
.lcomm rs, 4096

/* vi: set ft=arm64asm : */
