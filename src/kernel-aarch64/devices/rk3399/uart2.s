.section .text

.equ rk3399_uart2_base_addr, 0xff1a0000

/** rk3399_uart2_init: Initializes UART2 as expected by other functions
 *
 * Effects:
 * - Twiddles UART2 state
 * - Trashes x0
 */
.global rk3399_uart2_init
rk3399_uart2_init:
	ldr x0, =rk3399_uart2_base_addr
	strb wzr, [x0, #0x04]
	strb wzr, [x0, #0x08]
	ret

/** rk3399_uart2_read_line: Reads a line from UART2, with echoing.
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
 * - Trashes x2, x3, x4, x5
 *
 * Temporaries:
 *   x0: Address of next location to write to
 *   x1: Number of characters read
 *   x2: Length of the buffer
 *   x3: UART base address
 *   x4: UART_RBR contents (last character read)
 *   x5: UART_LSR contents
 */
.global rk3399_uart2_read_line
rk3399_uart2_read_line:
	ldr x3, =rk3399_uart2_base_addr
	mov x2, x1
	mov x1, xzr

rk3399_uart2_read_line.loop:
	cmp x1, x2
	b.eq rk3399_uart2_read_line.end

rk3399_uart2_read_line.wait_for_rx_ok:
	ldrb w5, [x3, #0x14]
	tbz w5, 0, rk3399_uart2_read_line.wait_for_rx_ok

	ldrb w4, [x3]
	strb w4, [x0, x1]

rk3399_uart2_read_line.wait_for_tx_ok:
	ldrb w5, [x3, #0x14]
	tbz w5, 5, rk3399_uart2_read_line.wait_for_tx_ok

	strb w4, [x3]

	cmp w4, '\n'
	b.eq rk3399_uart2_read_line.end
	cmp w4, '\r'
	b.eq rk3399_uart2_read_line.end
	cmp w4, #127
	b.eq rk3399_uart2_read_line.backspace

	add x1, x1, #1
	b rk3399_uart2_read_line.loop

rk3399_uart2_read_line.end:
	/* This should... probably be refactored. Don't want to require extra
	 * space for the link register, though... */
rk3399_uart2_read_line.wait_for_tx_ok_2:
	ldrb w5, [x3, #0x14]
	tbz w5, 5, rk3399_uart2_read_line.wait_for_tx_ok_2

	mov x4, '\r'
	strb w4, [x3]

rk3399_uart2_read_line.wait_for_tx_ok_3:
	ldrb w5, [x3, #0x14]
	tbz w5, 5, rk3399_uart2_read_line.wait_for_tx_ok_3

	mov x4, '\n'
	strb w4, [x3]

	ret

rk3399_uart2_read_line.backspace:
	sub x1, x1, #1
	b rk3399_uart2_read_line.loop

/** rk3399_uart2_write_string: Writes a string to UART2
 *
 * Input:
 *   x0: Address of first character of string
 *   x1: Length of string
 *
 * Effects:
 * - Blocks until the write is complete
 * - Writes to UART2
 * - Trashes x0, x1, x2, x3
 *
 * Temporaries:
 *   x0: Address of the character to be written next
 *   x1: Number of bytes remaining to write
 *   x2: UART base address
 *   x3: UART_LSR contents or character being written
 */
.global rk3399_uart2_write_string
rk3399_uart2_write_string:
	ldr x2, =rk3399_uart2_base_addr

rk3399_uart2_write_string.loop:
	cbz x1, rk3399_uart2_write_string.end

rk3399_uart2_write_string.wait_for_tx_ok:
	ldrb w3, [x2, #0x14]
	tbz w3, 5, rk3399_uart2_write_string.wait_for_tx_ok

	ldrb w3, [x0], #1
	sub x1, x1, 1
	strb w3, [x2]

	b rk3399_uart2_write_string.loop
rk3399_uart2_write_string.end:
	ret

/* vi: set ft=arm64asm : */
