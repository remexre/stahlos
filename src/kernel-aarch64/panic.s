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
defpanic todo
defpanic init_exited
defpanic out_of_data_stack
defpanic out_of_return_stack
defpanic underflow_data_stack
defpanic underflow_return_stack
defpanic end_of_source_when_parsing
defpanic word_not_found

loop_forever:
	wfi
	b loop_forever

.section .rodata

panic_str: .ascii "panic!\n"
.equ panic_str.len, . - panic_str

forth: .ascii "forth panic\n"
.equ forth.len, . - forth

todo: .ascii "TODO\n"
.equ todo.len, . - todo

init_exited: .ascii "init exited\n"
.equ init_exited.len, . - init_exited

out_of_data_stack: .ascii "out of data stack\n"
.equ out_of_data_stack.len, . - out_of_data_stack

out_of_return_stack: .ascii "out of return stack\n"
.equ out_of_return_stack.len, . - out_of_return_stack

underflow_data_stack: .ascii "data stack underflow\n"
.equ underflow_data_stack.len, . - underflow_data_stack

underflow_return_stack: .ascii "return stack underflow\n"
.equ underflow_return_stack.len, . - underflow_return_stack

end_of_source_when_parsing: .ascii "end of source when parsing\n"
.equ end_of_source_when_parsing.len, . - end_of_source_when_parsing

word_not_found: .ascii "word not found\n"
.equ word_not_found.len, . - word_not_found

/* vi: set ft=arm64asm : */
