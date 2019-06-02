bits 64

%include "src/amd64/macros.inc"
%include "src/amd64/kernel/macros.inc"

extern ipb.here
extern underflow

global forth_docolon.impl
global forth_last_builtin

[section .forth_builtins]

;;; Architecture-dependent stuff

defcode bochs_bp, "BOCHS-BP"
	dbg `BOCHS-BP word\n`
endcode

defcode outb, "OUTB", 2
	mov dx, bx
	pop rax
	pop rbx
	out dx, al
endcode

;;; Forth "deep builtins"

defcode allot, "ALLOT", 1
	add [ipb.here], rbx
	pop rbx
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

defcode dup, "DUP", 1
	push rbx
endcode

defcode exit, "EXIT"
	mov rsi, [rbp]
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
	jnz .end
	jmp rax
.end:
endcode

defcode literal_impl, "(LITERAL)"
	push rbx
	lodsq
	mov rbx, rax
endcode

defcode source, "SOURCE", 0
	push rbx
	mov rbx, [r15+16]
	push rbx
	mov rbx, [r15+24]
endcode

defcode store, "!", 2
	mov rax, [rsp]
	mov [rbx], rax
	add rsp, 8
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
