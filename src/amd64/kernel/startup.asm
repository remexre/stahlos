bits 64

extern interpret

global forth_cold

[section .text]

; Performs a cold startup of the Forth system.
forth_cold:
	; xchg bx, bx
	mov r8, forth_startup
	mov r9, forth_startup.len
	call interpret
	; xchg bx, bx
	mov r8, forth_std
	mov r9, forth_std.len
	call interpret
	; xchg bx, bx
	ret

[section .forth_code]

forth_startup:
	incbin "src/amd64/forth/startup.f"
.len equ $-forth_startup

forth_std:
	incbin "src/amd64/forth/std.f"
.len equ $-forth_std

; vi: cc=80 ft=nasm
