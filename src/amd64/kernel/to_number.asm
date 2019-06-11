bits 64

global to_number

[section .forth_builtins]

; This is in its own file so it's easier to test on a host system This is also
; the reason for the odd ABI (which is designed to be sane to call from the
; System V ABI). See src/utils/to_number_tests.c.

; len = rcx
; addr = rdx
; jumps to rdi afterwards.
; success flag is in rax as a qword, number is written to [r8] as a qword.
to_number:
	jmp rdi

; vi: cc=80 ft=nasm
