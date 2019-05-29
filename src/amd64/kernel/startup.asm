bits 64

extern cold_exited
extern interpret

extern forth_bochs_bp.cfa
extern forth_exit.cfa

global forth_cold

[section .text]

; Performs a cold startup of the Forth system.
forth_cold:
	xchg bx, bx
	sub rbp, 8
	mov qword [rbp], .after
	mov rsi, .pfa
	lodsq
	jmp rax
.after: dq cold_exited
.pfa:
	dq forth_bochs_bp.cfa
	dq forth_exit.cfa

[section .forth_code]

cold_code:
	incbin "src/amd64/forth/std.f"
	db 0x0a
	incbin "src/amd64/forth/startup.f"
.len equ $-cold_code

; vi: cc=80 ft=nasm
