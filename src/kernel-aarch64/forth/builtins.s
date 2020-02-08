.section .text

.macro defword label, name, flags=0
.global \label
\label\().padding:
.fill (258 - (\label - \label\().name)) % 4
\label\().header:
	.quad last_defword
	.byte \flags
	.byte \label - \label\().name
\label\().name:
	.ascii "\name"
\label:
.set last_defword, \label\().header
.endm
.set last_defword, 0

.macro pop
	cbz x12, panic.underflow_data_stack
	sub x12, x12, 8
	ldr x10, [x11, x12]
.endm

.macro push
	cmp x12, x13
	b.ge panic.out_of_data_stack
	str x10, [x11, x12]
	add x12, x12, 8
.endm

.macro rpop
	cbz x16, panic.underflow_return_stack
	sub x16, x16, 8
	ldr x14, [x15, x16]
.endm

.macro rpush
	cmp x16, x17
	b.ge panic.out_of_return_stack
	str x14, [x15, x16]
	add x16, x16, 8
.endm

.macro next
	ldr x0, [x18], #8
	br x0
.endm

defword forth_cr, "CR"
	bl format_write_newline
	next

defword forth_dot_dec, ".DEC"
	mov x0, x10
	pop
	bl format_write_num_sd
	next

defword forth_dot_hex, ".HEX"
	mov x0, x10
	pop
	bl format_write_num_ux
	next

defword forth_drop, "DROP"
	pop
	next

defword forth_dup, "DUP"
	push
	next

defword forth_emit, "EMIT"
	str x10, [x11]
	pop
	mov x0, x11
	mov x1, 1
	bl format_write_string
	next

defword forth_false, "FALSE"
	push
	mov x10, 0
	next

defword forth_find_header, "FIND-HEADER"
	/* TODO(safety): Check stack depth
	 * x0: target name length
	 * x1: target name pointer
	 * x2: current header name chars remaining
	 * x3: target name current position
	 * x4: current header name current position
	 * x5: target current char
	 * x6: current header name current char
	 * x10: current dictionary link pointer
	 */
	mov x0, x10
	pop
	mov x1, x10
	mov x10, x19
forth_find_header.next_header:
	cbz x10, forth_find_header.end
	ldr x10, [x10]

	ldrb w2, [x10, #9]
	cmp x2, x0
	b.ne forth_find_header.next_header

	mov x3, x1
	add x4, x10, #10

forth_find_header.strcmp_loop:
	cbz x2, forth_find_header.end
	ldrb w5, [x3], #1
	ldrb w6, [x4], #1
	cmp w5, w6
	b.ne forth_find_header.next_header
	sub x2, x2, 1
	b forth_find_header.strcmp_loop

forth_find_header.end:
	next

defword forth_from_rstack, "R>"
	push
	mov x10, x14
	rpop
	next

defword forth_impl_branch, "(BRANCH)"
	ldr x18, [x18]
	next

defword forth_impl_branch_zero, "(BRANCH0)"
	mov x0, x10
	ldr x1, [x18], #8
	pop
	cbnz x0, forth_impl_branch_zero.end
	mov x18, x1
forth_impl_branch_zero.end:
	next

defword forth_impl_colon, "(:)" /* aka enter, aka docolon */
	rpush
	mov x14, x18
	add x18, x0, 4
	next

defword forth_impl_literal, "(LITERAL)"
	push
	ldr x10, [x18], #8
	next

defword forth_impl_semicolon, "(;)" /* aka exit */
	mov x18, x14
	rpop
	next

defword forth_load_char, "C@"
	ldrb w10, [x10]
	next

defword forth_not, "NOT"
	tst x10, x10
	mov x10, xzr /* NZCV-preserving */
	b.ne forth_not.zero
	mvn x10, x10
forth_not.zero:
	next

defword forth_one_minus, "1-"
	sub x10, x10, 1
	next

defword forth_one_plus, "1+"
	add x10, x10, 1
	next

defword forth_over, "OVER"
	/* TODO(safety): Check stack depth */
	push
	add x0, x11, x12
	ldr x10, [x0, #-16]
	next

defword forth_panic, "PANIC"
	b panic.forth

defword forth_parse, "PARSE"
	/* x0: delim char
	 * x1: string base
	 * x2: string total length
	 * x3: string initial offset
	 * x4: current string pointer
	 * x5: characters remaining in string
	 * x6: current character
	 */
	and x0, x10, #0xff
	ldr x1, [x19, #8]
	ldr x2, [x19, #16]
	ldr x3, [x19, #24]
	add x4, x1, x3
	sub x5, x2, x3
	mov x10, x4
	push
forth_parse.loop:
	cbz x5, forth_parse.loopend
	sub x5, x5, #1
	ldrb w6, [x4], #1
	cmp w6, w0
	b.ne forth_parse.loop
forth_parse.loopend:
	sub x10, x4, x10
	add x3, x3, x10
	str x3, [x19, #24]
	cbz x10, forth_parse.end
	sub x10, x10, #1
forth_parse.end:
	next

defword forth_set_source, "SET-SOURCE"
	str x10, [x19, #24]
	pop
	str x10, [x19, #16]
	pop
	str x10, [x19, #8]
	pop
	next

defword forth_skip_spaces, "SKIP-SPACES"
	/* x0: string base
	 * x1: string total length
	 * x2: string initial offset
	 * x3: current character
	 */
	ldr x0, [x19, #8]
	ldr x1, [x19, #16]
	ldr x2, [x19, #24]
forth_skip_spaces.loop:
	cmp x1, x2
	b.eq forth_skip_spaces.end
	ldrb w3, [x0, x2]
	cmp w3, #0x20
	b.ne forth_skip_spaces.end
	add x2, x2, 1
	b forth_skip_spaces.loop
forth_skip_spaces.end:
	str x2, [x19, #24]
	next

defword forth_source, "SOURCE"
	push
	ldr x10, [x19, #8]
	push
	ldr x10, [x19, #16]
	push
	ldr x10, [x19, #24]
	next

defword forth_swap, "SWAP"
	/* TODO(safety): Check stack depth */
	add x2, x11, x12
	ldr x1, [x2, #-8]
	str x10, [x2, #-8]
	mov x10, x1
	next

defword forth_to_rstack, ">R"
	rpush
	mov x14, x10
	pop
	next

defword forth_true, "TRUE"
	push
	mvn x10, xzr
	next

defword forth_two_from_rstack, "2R>"
	push
	mov x0, x14
	rpop
	mov x10, x14
	push
	rpop
	mov x10, x0
	next

defword forth_two_to_rstack, "2>R"
	rpush
	mov x0, x10
	pop
	mov x14, x10
	rpush
	pop
	mov x14, x0
	next

.global forth.last_builtin_header
.equiv forth.last_builtin_header, last_defword

/* vi: set ft=arm64asm : */
