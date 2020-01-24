.section .text

/* init_epoll: Initializes the global epoll instance.
 *
 * Effects:
 * - Initializes the global epoll instance.
 */
.global init_epoll
init_epoll:
	mov x0, #0
	mov x1, #0
	mov x8, #20
	svc #0

	tbz x0, #63, =L0

	ldr x1, =epoll_fd
	str x0, [x1]
	ret
0:
	bl panic

/* panic: Abnormally terminates the process.
 *
 * Effects:
 * - Terminates the process, or enters the debugger if one is attached
 */
panic:
	brk #0

.section .bss

.global epoll_fd /* DEBUG */
.lcomm epoll_fd, 8

/* vi: set ft=arm64asm : */
