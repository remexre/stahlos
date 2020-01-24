.section .text

/* conv_number_bin: Converts an unsigned number to binary.
 *
 * Arguments:
 *   x0: Number to convert
 *
 * Return:
 *   x0: Length of the string
 *
 * Effects:
 * - Fills conv_buffer with the string
 * - Trashes x9
 */
.global conv_number_bin
conv_number_bin:
	mov x9, #0
	ret

/* conv_number_dec: Converts a signed number to decimal.
 *
 * Arguments:
 *   x0: Number to convert
 *
 * Return:
 *   x0: Length of the string
 *
 * Effects:
 * - Fills conv_buffer with the string
 * - Trashes x9
 */
.global conv_number_dec
conv_number_dec:
	mov x9, #0
	ret

/* flush_icache_range: Flushes the instruction cache lines convering the given
 * address range.
 *
 * Arguments:
 *   x0: Start Address
 *   x1: End Address (Exclusive)
 *
 * Effects:
 * - Trashes x0, x9
 * - Flushes the instruction cache between the start and end addresses
 */
.global flush_icache_range
flush_icache_range:
	ldr x9, =icache_line_size
0:
	cmp x0, x1
	bls =1
	bl flush_icache_addr
	add x0, x0, x9
	b =0
1:
	ret

/* flush_icache_addr: Flushes the instruction cache line containing the given address.
 *
 * Arguments:
 *   x0: Instruction Cache Address
 *
 * Effects:
 * - Flushes the instruction cache line containing the address
 */
flush_icache_addr:
	/* TODO: Right now, this just flushes the whole icache... */
	ic ialluis
	ret

.section .bss

.global conv_buffer /* DEBUG */
.lcomm conv_buffer, 64
.global icache_line_size /* DEBUG */
.lcomm icache_line_size, 8

/* vi: set ft=arm64asm : */
