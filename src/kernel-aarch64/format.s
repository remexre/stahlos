.section .text

/** format_init: Sets up formatting functions.
 *
 * Effects:
 * - Sets the format input/output devices
 * - Trashes x0-x9
 */
.global format_init
format_init:
	ldr x0, =stdin
	ldr x1, =rk3399_uart2_read_line
	str x1, [x0]

	ldr x0, =stdout
	ldr x1, =rk3399_uart2_write_string
	str x1, [x0]

	/* tail call */
	b rk3399_uart2_init

/** format_read_line: Reads a line from the input device, with echoing.
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
 * - Reads from the input device
 * - Writes to the provided buffer
 * - Trashes x0, x2-x9
 */
.global format_read_line
format_read_line:
	ldr x2, =stdin
	ldr x2, [x2]
	br x2

/** format_write_newline: Writes a newline to the output device.
 *
 * Side Effects:
 * - Blocks until the write is complete
 * - Writes to the output device
 * - Trashes x0-x9
 */
.global format_write_newline
format_write_newline:
	ldr x0, =newline
	mov x1, newline.len
	b format_write_string

/** format_write_num_sd: Writes a signed number in base 10 to the output device.
 *
 * Input:
 *   x0: The number to print
 *
 * Side Effects:
 * - Blocks until the write is complete
 * - Writes to the output device
 * - Trashes x0-x9
 * - Uses temporary storage; is NOT re-entrant
 */
.global format_write_num_sd
format_write_num_sd:
	b panic

/** format_write_num_ux: Writes an unsigned number in base 16 to the output device.
 *
 * Input:
 *   x0: The number to print
 *
 * Side Effects:
 * - Blocks until the write is complete
 * - Writes to the output device
 * - Trashes x0-x9
 * - Uses temporary storage; is NOT re-entrant
 *
 * Temporaries (until format_write_string call):
 * - x1: Number of characters to print after the current one
 * - x2: Current nybble being printed, as a number, address, or char
 * - x3: Address of the formatting table
 * - x4: Address of the number buffer
 */
.global format_write_num_ux
format_write_num_ux:
	mov x1, 15
	ldr x3, =format_write_num_ux.table
	ldr x4, =num_buf

format_write_num_ux.loop:
	and x2, x0, 15
	lsr x0, x0, 4
	ldrb w2, [x3, x2]
	strb w2, [x4, x1]

	cbz x1, format_write_num_ux.end
	sub x1, x1, 1
	b format_write_num_ux.loop

format_write_num_ux.end:
	mov x0, x4
	mov x1, 16
	b format_write_string

/** format_write_string: Writes a string to the output device.
 *
 * Input:
 *   x0: Address of first character of string
 *   x1: Length of string
 *
 * Side Effects:
 * - Blocks until the write is complete
 * - Writes to the output device
 * - Trashes x0-x9
 */
.global format_write_string
format_write_string:
	ldr x2, =stdout
	ldr x2, [x2]
	br x2

.section .rodata

format_write_num_ux.table: .string "0123456789abcdef"

newline: .string "\r\n"
.equ newline.len, . - newline

.section .bss

.comm stdin, 8
.comm stdout, 8
.comm num_buf, 20

/* vi: set ft=arm64asm : */
