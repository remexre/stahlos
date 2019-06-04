bits 64

%include "src/amd64/macros.inc"
%include "src/amd64/kernel/macros.inc"

extern ipb.here
extern underflow
extern underflow_return

global forth_docolon.impl
global forth_last_builtin

[section .forth_builtins]

;;; Architecture-dependent stuff

defcode bochs_bp, "BOCHS-BP"
	dbg `BOCHS-BP\n`
endcode

defcode outb, "OUTB", 2
	mov dx, bx
	pop rax
	pop rbx
	out dx, al
endcode

;;; Forth "deep builtins"

defcode add, "+", 2
	pop rax
	add rbx, rax
endcode

; This is just in assembly for convenience (oddly enough). 3 instructions of
; assembly versus 7 words (shortest I could find was DUP >R - SWAP R> + SWAP).
defcode adjust_string, "/STRING", 3
	sub [rsp], rbx
	add [rsp+8], rbx
	pop rbx
endcode

defcode allot, "ALLOT", 1
	add [ipb.here], rbx
	pop rbx
endcode

defcode base_decimal, "DECIMAL"
	and byte [r15+40], 0xfe
endcode

defcode base_hex, "HEX"
	or byte [r15+40], 0x01
endcode

defcode decr, "1-", 1
	dec rbx
endcode

defcode depth, "DEPTH"
	push rbx
	mov rbx, r13
	sub rbx, rsp
	shr rbx, 3
endcode

defcode docolon, "((DOCOLON))"
	jmp near .impl
.jmp_len equ $ - .cfa
.pfa:
	dq forth_literal_impl.cfa
	dq .impl
	dq forth_exit.cfa
.impl:
	sub rbp, 8
	mov [rbp], rsi
	lea rsi, [rax+.jmp_len]
endcode

defcode drop, "DROP", 1
	pop rbx
endcode

defcode dup, "DUP", 1
	push rbx
endcode

defcode exit, "EXIT"
	; Check for return underflow.
	lea rcx, [rbp+8]
	cmp r14, rcx
	jb underflow_return

	mov rsi, [rbp]
	add rbp, 8
endcode

defcode fetch, "@", 1
	mov rbx, [rbx]
endcode

defcode from_r, "R>"
	; Check for return underflow.
	lea rcx, [rbp+8]
	cmp r14, rcx
	jb underflow_return

	push rbx
	mov rbx, [rbp]
	add rbp, 8
endcode

defcode here, "HERE"
	push rbx
	mov rbx, [ipb.here]
endcode

defcode if_impl, "(IF)", 1
	test rbx, rbx
	pop rbx
	lodsq
	cmovz rsi, rax
endcode

defcode incr, "1+", 1
	inc rbx
endcode

defcode jump, "(JUMP)"
	lodsq
	mov rsi, rax
endcode

defcode literal_impl, "(LITERAL)"
	push rbx
	lodsq
	mov rbx, rax
endcode

defcode pick, "PICK", 1
	mov rbx, [rsp+rbx*8]
endcode

defcode s_to_d, "S>D", 1
	push rbx
	sar rbx, 31
endcode

defcode source_buffer, "(SOURCE-BUFFER)"
	push rbx
	lea rbx, [r15+16]
endcode

defcode source_length, "(SOURCE-LENGTH)"
	push rbx
	lea rbx, [r15+24]
endcode

defcode state_compile, "]"
	or byte [r15+40], 0x02
endcode

defcode state_interpret, "["
	and byte [r15+40], 0xfd
endcode

defcode store, "!", 2
	mov rax, [rsp]
	mov [rbx], rax
	add rsp, 8
	pop rbx
endcode

defcode sub, "-", 2
	pop rax
	sub rax, rbx
	mov rbx, rax
endcode

defcode swap, "SWAP"
	xchg [rsp], rbx
endcode

defcode to_in, ">IN"
	push rbx
	lea rbx, [r15+32]
endcode

defcode to_r, ">R", 1
	sub rbp, 8
	mov [rbp], rbx
	pop rbx
endcode

defcode user_pointer, "USER-POINTER"
	push rbx
	mov rbx, r15
endcode

; This is a smudged, no-name, no-op word, as a marker and safety guard.
defcode last_builtin, "", 0, 0x02
endcode

; vi: cc=80 ft=nasm
