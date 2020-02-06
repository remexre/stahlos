.section .text

.equ qemuvirt_uart_base_addr, 0x09000000

/** qemuvirt_uart_init: A no-op to comply with the expected interface.
 */
.global qemuvirt_uart_init
qemuvirt_uart_init:
	ret

/** qemuvirt_uart_read_line: Reads a line from the UART, with echoing.
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
.global qemuvirt_uart_read_line
qemuvirt_uart_read_line:
	ldr x3, =qemuvirt_uart_base_addr
	mov x2, x1
	mov x1, xzr

qemuvirt_uart_read_line.loop:
	cmp x1, x2
	b.eq qemuvirt_uart_read_line.end

qemuvirt_uart_read_line.wait_for_rx_ok:
	ldrb w5, [x3, #0x18]
	tbz w5, 6, qemuvirt_uart_read_line.wait_for_rx_ok

	ldrb w4, [x3]
	strb w4, [x0, x1]

qemuvirt_uart_read_line.wait_for_tx_ok:
	ldrb w5, [x3, #0x18]
	tbz w5, 7, qemuvirt_uart_read_line.wait_for_tx_ok

	strb w4, [x3]

	cmp w4, '\n'
	b.eq qemuvirt_uart_read_line.end
	cmp w4, '\r'
	b.eq qemuvirt_uart_read_line.end
	cmp w4, #127
	b.eq qemuvirt_uart_read_line.backspace

	add x1, x1, #1
	b qemuvirt_uart_read_line.loop

qemuvirt_uart_read_line.end:
	/* This should... probably be refactored. Don't want to require extra
	 * space for the link register, though... */
qemuvirt_uart_read_line.wait_for_tx_ok_2:
	ldrb w5, [x3, #0x18]
	tbz w5, 7, qemuvirt_uart_read_line.wait_for_tx_ok_2

	mov x4, '\r'
	strb w4, [x3]

qemuvirt_uart_read_line.wait_for_tx_ok_3:
	ldrb w5, [x3, #0x18]
	tbz w5, 7, qemuvirt_uart_read_line.wait_for_tx_ok_3

	mov x4, '\n'
	strb w4, [x3]

	ret

qemuvirt_uart_read_line.backspace:
	sub x1, x1, #1
	b qemuvirt_uart_read_line.loop

/** qemuvirt_uart_write_string: Writes a string to UART2
 *
 * Input:
 *   x0: Address of first character of string
 *   x1: Length of string
 *
 * Effects:
 * - Blocks until the write is complete
 * - Writes to UART2
 * - Trashes x0, x1, x2, x3, x4
 *
 * Temporaries:
 *   x0: Address of the character to be written next
 *   x1: Number of bytes remaining to write
 *   x2: UART base address
 *   x3: UART_LSR contents
 *   x4: character being written
 */
.global qemuvirt_uart_write_string
qemuvirt_uart_write_string:
	ldr x2, =qemuvirt_uart_base_addr

qemuvirt_uart_write_string.loop:
	cbz x1, qemuvirt_uart_write_string.end
	ldrb w4, [x0], #1
	sub x1, x1, 1

qemuvirt_uart_write_string.wait_for_tx_ok:
	ldrb w3, [x2, #0x18]
	tbz w3, 7, qemuvirt_uart_write_string.wait_for_tx_ok

	strb w4, [x2]

	cmp w4, '\n'
	b.eq qemuvirt_uart_write_string.newline

	b qemuvirt_uart_write_string.loop
qemuvirt_uart_write_string.end:
	ret

qemuvirt_uart_write_string.newline:
	mov w4, '\r'
	b qemuvirt_uart_write_string.wait_for_tx_ok

/* vi: set ft=arm64asm : */
