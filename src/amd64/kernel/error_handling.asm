bits 64

%include "src/amd64/macros.inc"

global cold_exited
global undefined_word
global underflow
global underflow_return

[section .text]

cold_exited:
	dbg `cold_exited loop\n`
	hlt
	jmp cold_exited

undefined_word:
	dbg `undefined_word loop\n`
	hlt
	jmp undefined_word

underflow:
	dbg `underflow loop\n`
	hlt
	jmp underflow

underflow_return:
	dbg `underflow_return loop\n`
	hlt
	jmp underflow_return

; vi: cc=80 ft=nasm
