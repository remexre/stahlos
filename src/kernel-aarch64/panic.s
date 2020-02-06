.section .text

.macro defpanic name
	.global panic.\name
	panic.\name:
		ldr x0, =\name
		mov x1, \name\().len
		ldr x30, =panic
		b format_write_string
.endm

.global panic
panic:
	ldr x0, =panic_str
	mov x1, panic_str.len
	ldr x30, =loop_forever
	b format_write_string
defpanic forth
defpanic out_of_data_stack
defpanic out_of_return_stack
defpanic underflow_data_stack
defpanic underflow_return_stack

loop_forever:
	wfe
	b loop_forever

.section .rodata

panic_str: .string "panic!\n"
.equ panic_str.len, . - panic_str

forth: .string "forth panic\n"
.equ forth.len, . - forth

out_of_data_stack: .string "out of data stack\n"
.equ out_of_data_stack.len, . - out_of_data_stack

out_of_return_stack: .string "out of return stack\n"
.equ out_of_return_stack.len, . - out_of_return_stack

underflow_data_stack: .string "data stack underflow\n"
.equ underflow_data_stack.len, . - underflow_data_stack

underflow_return_stack: .string "return stack underflow\n"
.equ underflow_return_stack.len, . - underflow_return_stack

/* vi: set ft=arm64asm : */
