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

defword forth_allot, "ALLOT"
	ldr x0, =data_space_ptr
	ldr x1, [x0]
	add x1, x10, x1
	pop
	str x1, [x0]
	next

defword forth_and, "AND"
	mov x0, x10
	pop
	and x10, x10, x0
	next

defword forth_boolify, "BOOLIFY"
	/* TODO(safety): Check stack depth */
	cbz x10, forth_boolify.zero
	mvn x10, xzr
forth_boolify.zero:
	next

defword forth_comma, ","
	ldr x0, =data_space_ptr
	ldr x1, [x0]
	str x10, [x1], #8
	pop
	str x1, [x0]
	next

defword forth_comma_char, "C,"
	ldr x0, =data_space_ptr
	ldr x1, [x0]
	strb w10, [x1], #1
	pop
	str x1, [x0]
	next

defword forth_compile_comma, "COMPILE,"
	ldr x0, =data_space_ptr
	ldr x1, [x0]
	str x10, [x1], #8
	pop
	str x1, [x0]
	next

defword forth_cr, "CR"
	bl format_write_newline
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

defword forth_execute, "EXECUTE"
	mov x0, x10
	pop
	br x0

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

defword forth_flags, "FLAGS"
	push
	ldr x10, [x19, #0x28]
	next

defword forth_from_rstack, "R>"
	push
	mov x10, x14
	rpop
	next

defword forth_header_to_flags, "HEADER>FLAGS"
	/* TODO(safety): Check stack depth */
	ldrb w10, [x10, #8]
	next

defword forth_header_to_xt, "HEADER>XT"
	/* TODO(safety): Check stack depth */
	ldrb w0, [x10, #9]
	add x10, x10, x0
	add x10, x10, #10
	next

defword forth_here, "HERE"
	push
	ldr x0, =data_space_ptr
	ldr x10, [x0]
	next

defword forth_immediate, "IMMEDIATE"
	ldr x0, [x19]
	ldrb w1, [x0, #8]
	orr w1, w1, #1
	strb w1, [x0, #8]
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
	add x18, x0, #16
	next

defword forth_impl_debug, "(DEBUG)"
	next

defword forth_impl_literal, "(LITERAL)"
	push
	ldr x10, [x18], #8
	next

defword forth_impl_semicolon, "(;)" /* aka exit */
	mov x18, x14
	rpop
	next

defword forth_impl_variable, "(VARIABLE)"
	push
	mov x10, x18
	/* TODO: Correctness? */
	next

defword forth_load_char, "C@"
	ldrb w10, [x10]
	next

defword forth_load_qword, "@"
	/* TODO(safety): Check stack depth */
	ldr x10, [x10]
	next

defword forth_minus, "-"
	/* TODO(safety): Check stack depth */
	mov x0, x10
	pop
	sub x10, x10, x0
	next

defword forth_mode_compile, "]"
	ldr x0, [x19, #0x28]
	and x0, x0, 254
	str x0, [x19, #0x28]
	next

defword forth_mode_interpret, "[", 1
	ldr x0, [x19, #0x28]
	orr x0, x0, 1
	str x0, [x19, #0x28]
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

defword forth_or, "OR"
	mov x0, x10
	pop
	orr x10, x10, x0
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

defword forth_parse_name, "PARSE-NAME"
	/* x0: string base
	 * x1: string total length
	 * x2: string initial offset
	 * x3: current character
	 */
	ldr x0, [x19, #8]
	ldr x1, [x19, #16]
	ldr x2, [x19, #24]
forth_parse_name.skip_space_loop:
	cmp x1, x2
	b.eq forth_parse_name.spaces_skipped
	ldrb w3, [x0, x2]
	cmp w3, #0x21
	b.hs forth_parse_name.spaces_skipped
	add x2, x2, 1
	b forth_parse_name.skip_space_loop
forth_parse_name.spaces_skipped:
	/* x0: start of string
	 * x1: remaining length
	 * x2: offset of start of string
	 * x3: current character
	 * x10: length so far
	 */
	sub x1, x1, x2
	push
	add x10, x0, x2
	mov x0, x10
	push
	mov x10, xzr
forth_parse_name.loop:
	cbz x1, forth_parse_name.end
	ldrb w3, [x0], #1
	cmp w3, #0x21
	b.lo forth_parse_name.end
	sub x1, x1, 1
	add x10, x10, 1
	b forth_parse_name.loop
forth_parse_name.end:
	add x2, x2, x10
	str x2, [x19, #24]
	next

defword forth_process_table, "PROCESS-TABLE"
	push
	mov x10, x19
	next

defword forth_set_does, "SET-DOES"
	/* x0: Address of DOES word's CFA / DOES word's XT
	 * x1: Snippet to do jump to absolute addr
	 * x2: Address of word, later addr of CFA
	 * x3: Length of word name (DOES, then target)
	 */
	mov x0, x10
	pop
	ldr x2, [x19]
	ldrb w3, [x2, #9]!
	add x2, x2, #1
	add x2, x2, x3
	/* TODO: Store a direct B instruction when possible? */
	ldr x1, =0xd61f012058000049 /* ldr x9, [pc, 8]; br x9 */
	str x1, [x2]
	str x0, [x2, #8]

	/* TODO: Probably clear the icaches? */

	next

defword forth_set_source, "SET-SOURCE"
	str x10, [x19, #24]
	pop
	str x10, [x19, #16]
	pop
	str x10, [x19, #8]
	pop
	next

defword forth_source, "SOURCE"
	push
	ldr x10, [x19, #8]
	push
	ldr x10, [x19, #16]
	push
	ldr x10, [x19, #24]
	next

defword forth_store_qword, "!"
	mov x0, x10
	pop
	mov x1, x10
	pop
	str x1, [x0]
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

.section .bss

.comm data_space_ptr, 8

/* vi: set ft=arm64asm : */
