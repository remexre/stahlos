.section .text

/** to_number: Parses a number.
 *
 * Notes:
 *   Supported number format: [#$]?-?[0-9]+
 *   # indicates decimal, $ indicates hex
 *
 * Input:
 *   x0: The address of the string to parse
 *   x1: The length of the string to parse
 *   x2: The default base: zero if decimal, nonzero if hex
 *
 * Output:
 *   x3: The parsed number
 *   x4: Status Code: zero if invalid, nonzero if valid
 *
 * Effects:
 * - Trashes x0, x1, x4, x5
 *
 * Temporaries:
 *   x4: The character read from the string.
 *   x5: Negative flag: zero if negative, nonzero if positive
 */
.global to_number
to_number:

to_number.check_base_override:
	cbz x1, to_number.error
	ldrb w4, [x0]
	cmp w4, '#'
	b.eq to_number.override_hex
	cmp w4, '$'
	b.eq to_number.override_dec

to_number.check_negative:
	mvn x5, xzr
	cbz x1, to_number.error
	ldrb w4, [x0]
	cmp w4, '-'
	b.ne to_number.branch_on_base
	add x0, x0, 1
	sub x1, x1, 1
	mov x5, xzr

to_number.branch_on_base:
	cbz x1, to_number.error
	cbz x2, to_number.loop_dec

to_number.loop_hex:
	cbz x1, to_number.ok
	ldrb w4, [x0], #1
	/* TODO */
	b to_number.loop_hex

to_number.loop_dec:
	cbz x1, to_number.ok
	ldrb w4, [x0], #1
	cmp w4, '0'
	b.lo to_number.error
	cmp w4, ':'
	b.hs to_number.error
	sub w4, w4, '0'
	add x3, x3, x3, lsl #2
	add x3, x4, x3, lsl #1
	b to_number.loop_dec

to_number.ok:
	mvn x4, xzr
	ret
to_number.error:
	mov x4, xzr
	ret

to_number.override_dec:
	mov x2, xzr
	add x0, x0, 1
	sub x1, x1, 1
	b to_number.check_negative

to_number.override_hex:
	mvn x2, xzr
	add x0, x0, 1
	sub x1, x1, 1
	b to_number.check_negative

/* vi: set ft=arm64asm : */
