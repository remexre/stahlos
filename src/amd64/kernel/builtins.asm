bits 64

extern underflow

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

forth_bochs_debug:
	dd 0
	db 0x00, 11, "BOCHS-DEBUG"
.cfa:
	xchg bx, bx
	NEXT

; This is a smudged, no-name, no-op word, mainly as a marker.
forth_last_builtin:
	dd 0
	db 0x02, 0
.cfa:
	NEXT

; vi: cc=80 ft=nasm
