bits 64

%include "src/amd64/macros.inc"

global cold_exited
global underflow
global underflow_return

[section .text]

cold_exited:
	dbg `cold_exited loop\n`
	jmp cold_exited

underflow:
	dbg `underflow loop\n`
	jmp underflow

underflow_return:
	dbg `underflow_return loop\n`
	jmp underflow_return

; vi: cc=80 ft=nasm
