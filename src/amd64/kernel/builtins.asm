bits 64

extern ipb.here
extern underflow

global forth_bochs_bp.cfa
global forth_exit.cfa
global forth_last_builtin

; Checks if the given number of arguments can be popped. Trashes rcx.
%macro FORTH_POP_CHK 1
	lea rcx, [r13+8-(%1*8)]
	cmp rcx, rsp
	jbe underflow
%endmacro

%macro NEXT 0
	lodsq
	jmp rax
%endmacro

[section .forth_builtins]

forth_bochs_bp:
	dd 0
	db 0x00, 8, "BOCHS-BP"
.cfa:
	xchg bx, bx
	NEXT

forth_allot:
	dd forth_bochs_bp
	db 0x00, 5, "ALLOT"
.cfa:
	FORTH_POP_CHK 1
	lock xadd [ipb.here], rbx
	pop rbx
	NEXT

forth_exit:
	dd forth_allot
	db 0x00, 4, "EXIT"
.cfa:
	mov rsi, [rbp]
	add rbp, 8
	NEXT

forth_here:
	dd forth_exit
	db 0x00, 4, "HERE"
.cfa:
	push rbx
	xor rbx, rbx
	lock xadd [ipb.here], rbx ; It /should/ be possible to just do a normal
	; mov rbx, [ipb.here], per section 8.1.1 of Intel 64 and IA-32
	; Architectures Developer's Manual: Vol. 3A. However, it's not worth the
	; worry imo.
	NEXT

; This is a smudged, no-name, no-op word, mainly as a marker.
forth_last_builtin:
	dd forth_here
	db 0x02, 0
.cfa:
	NEXT

; vi: cc=80 ft=nasm
