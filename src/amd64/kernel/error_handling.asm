bits 64

%include "src/amd64/macros.inc"

global cold_exited
global underflow

[section .text]

cold_exited:
	dbg `cold_exited loop\n`
	jmp cold_exited

underflow:
	dbg `underflow loop\n`
	jmp underflow

; vi: cc=80 ft=nasm
