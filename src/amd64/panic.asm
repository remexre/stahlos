bits 64

%include "src/amd64/macros.inc"

global panic

[section .text]

panic:
	dbg `entered panic\n`

	mov rcx, .strlen
	mov rsi, .str
.loop:
	lodsb
	out 0xe9, al
	loop .loop
.halt:
	cli
	hlt
	jmp .halt

.str: db "PANIC!"
.strlen equ $ - .str

; vi: cc=80 ft=nasm
