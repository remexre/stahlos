bits 64

%include "src/amd64/macros.inc"

global cold_exited
global init_kill
global no_code_field
global undefined_word
global underflow
global underflow_return

[section .text]

cold_exited:
	dbg `cold_exited loop\n`
	cli
	hlt
	jmp cold_exited

init_kill:
	dbg `init_kill loop\n`
	cli
	hlt
	jmp init_kill

no_code_field:
	dbg `no_code_field loop\n`
	cli
	hlt
	jmp cold_exited

undefined_word:
	dbg `undefined_word loop\n`
	cli
	hlt
	jmp undefined_word

underflow:
	dbg `underflow loop\n`
	cli
	hlt
	jmp underflow

underflow_return:
	dbg `underflow_return loop\n`
	cli
	hlt
	jmp underflow_return

; vi: cc=80 ft=nasm
