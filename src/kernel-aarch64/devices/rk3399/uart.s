.section .text

/** uart_init: Initializes UART2 as expected by other functions in this file
 *
 * Effects:
 * - Twiddles UART2 state
 * - Trashes x0
 */
.global uart_init
uart_init:
	ldr x0, =0xff1a0000
	strb wzr, [x0, #0x04]
	strb wzr, [x0, #0x08]
	ret

/** uart_get_line: Reads a line from UART2
 *
 * Input:
 *   x0: Address of buffer
 *   x1: Length of buffer
 *
 * Output:
 *   x1: Number of characters read (excluding newline)
 *
 * Effects:
 * - Blocks until a newline is received or the buffer is filled
 * - Reads from UART2
 * - Writes to the provided buffer
 * - Trashes x0, x2, x3, and x4
 *
 * Temporaries:
 *   x0: Address of next location to write to
 *   x1: Number of characters read
 *   x2: Length of the buffer
 *   x3: UART_RBR or UART_LSR contents
 *   x4: UART base address
 */
.global uart_get_line
uart_get_line:
	ldr x4, =0xff1a0000
	mov x2, x1

	cmp x1, x2
	b.eq uart_get_line.end

uart_get_line.loop:
	ldr x3, [x4, #0x14]
	tbz x3, 0, uart_get_line.loop

	ldr w3, [x4]
	cmp w3, #0x0a
	b.eq uart_get_line.end

	add x1, x1, #1
	strb w3, [x0], #1
	b uart_get_line.loop

uart_get_line.end:
	ret
	

/* vi: set ft=arm64asm : */
