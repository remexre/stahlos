bits 64

global cold_exited
global underflow

[section .text]

cold_exited:
	xchg bx, bx
	jmp cold_exited

underflow:
	xchg bx, bx
	jmp underflow

; vi: cc=80 ft=nasm
