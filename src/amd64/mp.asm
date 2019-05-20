bits 64

global mp_init

[section .text]

; Sets up the global multitasking-related information.
mp_init:
	xchg bx, bx
	jmp mp_init

; vi: cc=80 ft=nasm
