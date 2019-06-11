bits 64

%include "src/amd64/macros.inc"
%include "src/amd64/kernel/macros.inc"

extern ipb
extern ipb.here
extern to_number
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

defcode add_store, "+!", 2
	pop rax
	add [rbx], rax
	pop rbx
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

defcode and, "AND", 2
	pop rax
	and rbx, rax
endcode

defcode arith_right_shift, "ARSHIFT", 2
	mov rcx, rbx
	pop rbx
	sar rbx, cl
endcode

defcode base_decimal, "DECIMAL"
	and byte [r15+48], 0xfe
endcode

defcode base_hex, "HEX"
	or byte [r15+48], 0x01
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

defcode dovar, "((DOVAR))"
	push rbx
	lea rbx, [rax+5]
endcode

defcode drop, "DROP", 1
	pop rbx
endcode

defcode dup, "DUP", 1
	push rbx
endcode

defcode dup_nonzero, "?DUP", 1
	test rbx, rbx
	jz .exit
	push rbx
.exit:
endcode

defcode equal, "=", 2
	pop rax
	xor rdx, rdx
	cmp rax, rbx
	setnz dl
	dec rdx
	mov rbx, rdx
endcode

defcode execute, "EXECUTE", 1
	mov rax, rbx
	pop rbx
	jmp rax
endcode

defcode exit, "EXIT"
	; Check for return underflow.
	lea rcx, [rbp+8]
	cmp r14, rcx
	jb underflow_return

	mov rsi, [rbp]
	add rbp, 8
endcode

defcode false, "FALSE"
	push rbx
	xor rbx, rbx
endcode

defcode fetch, "@", 1
	mov rbx, [rbx]
endcode

defcode fetch_char, "C@", 1
	movzx rbx, byte [rbx]
endcode

defcode fetch_dword, "D@", 1
	mov ebx, [rbx]
endcode

defcode fetch_word, "W@", 1
	movzx rbx, word [rbx]
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

defcode from_r_2, "R>"
	; Check for return underflow.
	lea rcx, [rbp+16]
	cmp r14, rcx
	jb underflow_return

	sub rsp, 16
	mov [rsp+8], rbx
	mov rax, [rbp+8]
	mov [rsp], rax
	mov rbx, [rbp]
	add rbp, 16
endcode

defcode get_base, "GET-BASE"
	test byte [r15+48], 0x01
	push rbx
	mov rbx, 10
	mov rax, 16
	cmovnz rbx, rax
endcode

defcode get_state, "GET-STATE"
	push rbx
	xor rbx, rbx
	test byte [r15+48], 0x02
	setz bl
	dec rbx
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

defcode invert, "INVERT", 1
	not rbx
endcode

defcode ipb, "IPB"
	push rbx
	mov rbx, ipb
endcode

defcode jump, "(JUMP)"
	lodsq
	mov rsi, rax
endcode

defcode left_shift, "LSHIFT", 2
	mov rcx, rbx
	pop rbx
	shl rbx, cl
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
	test byte [r15+48], 0x01
	mov rbx, 10
	mov rcx, 16
	cmovnz rbx, rcx ; rbx = base

	mov rcx, 32 ; rcx = current offset in str

.loop:
	xor rdx, rdx
	idiv rbx
	mov dl, [.chrs+rdx]
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
.chrs: db "0123456789abcdef"

defcode negate, "NEGATE", 1
	neg rbx
endcode

defcode noop, "NOOP"
	nop
endcode

defcode or, "OR", 2
	pop rax
	or rbx, rax
endcode

defcode pick, "PICK", 1
	lea rbx, [rsp+rbx*8]
	cmp r13, rbx
	jb underflow
	mov rbx, [rbx]
endcode

defcode r_fetch, "R@"
	; Check for return underflow.
	lea rcx, [rbp+8]
	cmp r14, rcx
	jb underflow_return

	push rbx
	mov rbx, [rbp]
endcode

defcode rev_rot, "-ROT", 3
	mov rax, [rsp+8]
	mov [rsp+8], rbx
	mov rbx, [rsp]
	mov [rsp], rax
endcode

defcode right_shift, "RSHIFT", 2
	mov rcx, rbx
	pop rbx
	shr rbx, cl
endcode

defcode rot, "ROT", 3
	mov rax, [rsp]
	mov [rsp], rbx
	mov rbx, [rsp+8]
	mov [rsp+8], rax
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
	or byte [r15+48], 0x02
endcode

defcode state_interpret, "[", 0, 0x01
	and byte [r15+48], 0xfd
endcode

defcode store, "!", 2
	pop rax
	mov [rbx], rax
	pop rbx
endcode

defcode store_char, "C!", 2
	pop rax
	mov [rbx], al
	pop rbx
endcode

defcode store_dword, "D!", 2
	pop rax
	mov [rbx], eax
	pop rbx
endcode

defcode store_word, "W!", 2
	pop rax
	mov [rbx], ax
	pop rbx
endcode

defcode streq, "STRING=", 4
	; ( addr1 len1 addr2 len2 ) mapped to
	; ( rdx   rcx  rdi   rax  )
	mov rdi, [rsp]
	mov rcx, [rsp+8]
	mov rdx, [rsp+16]
	mov rax, rbx
	add rsp, 24
	xor rbx, rbx

	cmp rax, rcx
	jne .fail

	xor r8, r8
.loop:
	cmp r8, rcx
	je .success

	mov al, [rdx+r8]
	mov r9b, [rdi+r8]
	cmp r9b, al
	jne .fail

	inc r8
	jmp .loop

.success:
	dec rbx
.fail:
endcode

defcode sub, "-", 2
	pop rax
	sub rax, rbx
	mov rbx, rax
endcode

defcode swap, "SWAP"
	xchg [rsp], rbx
endcode

defcode test_flag, "TEST-FLAG", 2
	pop rdx
	mov rcx, rbx
	mov rax, 1
	shl rax, cl
	xor rbx, rbx
	test rax, rdx
	jz .exit
	dec rbx
.exit:
endcode

defcode to_in, ">IN"
	push rbx
	lea rbx, [r15+32]
endcode

defcode to_number, ">NUMBER", 2
	; ( addr len -- num 1 | 0 )
	mov rdx, [rsp]
	mov rcx, rbx
	mov rdi, .after
	mov r12, rsp
	jmp to_number
.after:
	mov rbx, rax
	test rbx, rbx
	jnz .end
	add rsp, 8
.end:
endcode

defcode to_r, ">R", 1
	sub rbp, 8
	mov [rbp], rbx
	pop rbx
endcode

defcode to_r_2, "2>R", 2
	sub rbp, 16
	mov [rbp], rbx
	mov rax, [rsp]
	mov [rbp+8], rax
	mov rbx, [rsp+8]
	add rsp, 16
endcode

defcode true, "TRUE"
	push rbx
	xor rbx, rbx
	dec rbx
endcode

defcode u_greater, "U>", 2
	pop rax
	xor rdx, rdx
	cmp rax, rbx
	setbe dl
	dec rdx
	mov rbx, rdx
endcode

defcode u_less, "U<", 2
	pop rax
	xor rdx, rdx
	cmp rax, rbx
	setae dl
	dec rdx
	mov rbx, rdx
endcode

defcode user_pointer, "USER-POINTER"
	push rbx
	mov rbx, r15
endcode

; This is a no-name no-op word, as a marker and safety guard.
defcode last_builtin, "", 0
endcode

; vi: cc=80 ft=nasm
