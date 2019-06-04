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

defcode abs, "ABS", 1
	test rbx, rbx
	jns .end
	neg rbx
.end:
endcode

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
	dec rbx
endcode

defcode div_mod, "/MOD", 2
	mov rax, [rsp]
	xor rdx, rdx
	div rbx
	mov [rsp], rdx
	mov rbx, rax
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

defcode equal, "=", 2
	pop rax
	cmp rax, rbx
	je .eq
.ne:
	xor rbx, rbx
	lodsq
	jmp rax
.eq:
	xor rbx, rbx
	not rbx
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

defcode fetch_char, "C@", 1
	movzx rbx, byte [rbx]
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

defcode get_base, "GET-BASE"
	test byte [r15+40], 0x01
	push rbx
	mov rbx, 10
	mov rax, 16
	cmovnz rbx, rax
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

defcode n_to_str, "N>STR", 1
	mov rax, rbx ; rax = current val

	xor r8, r8
	test rax, rax
	jns .not_neg
	mov r8b, 1 ; r8b = negative flag
	neg rax

.not_neg:
	test byte [r15+40], 0x01
	mov rbx, 10
	mov rcx, 16
	cmovnz rbx, rcx ; rbx = base

	mov rcx, 32 ; rcx = current offset in str

.loop:
	xor rdx, rdx
	idiv rbx
	add dl, '0'
	mov [rcx+.buf-1], dl
	dec rcx

	test rax, rax
	jnz .loop

	test r8b, r8b
	jz .end

	; Add the '-'
	mov byte [rcx+.buf-1], '-'
	dec rcx

.end:
	lea rbx, [rcx+.buf]
	push rbx
	mov rbx, 32
	sub rbx, rcx
endcode
.buf: times 32 db 0

defcode pick, "PICK", 1
	lea rbx, [rsp+rbx*8]
	cmp r13, rbx
	jb underflow
	mov rbx, [rbx]
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
