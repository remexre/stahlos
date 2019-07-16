bits 64

%include "src/amd64/chacha20.asm"

global rotate_left_7
global quarter_round
global two_rounds
global twenty_rounds

extern memcpy

[section .text]

rotate_left_7:
	mov [rotate_left_7_tmp], edi
	movdqa xmm0, [rotate_left_7_tmp]
.body:
	chacha20_prold xmm0, 7, xmm4
	movdqa [rotate_left_7_tmp], xmm0
	mov rax, [rotate_left_7_tmp]
	ret

quarter_round:
.head:
	mov eax, [rdi]
	mov [quarter_round_tmp.a], eax
	mov eax, [rsi]
	mov [quarter_round_tmp.b], eax
	mov eax, [rdx]
	mov [quarter_round_tmp.c], eax
	mov eax, [rcx]
	mov [quarter_round_tmp.d], eax
	movdqa xmm0, [quarter_round_tmp.a]
	movdqa xmm1, [quarter_round_tmp.b]
	movdqa xmm2, [quarter_round_tmp.c]
	movdqa xmm3, [quarter_round_tmp.d]
	movdqa xmm5, [chacha20_mask8]
	movdqa xmm6, [chacha20_mask16]
.body:
	chacha20_qr xmm0, xmm1, xmm2, xmm3, xmm4, xmm5, xmm6
.tail:
	movdqa [quarter_round_tmp.a], xmm0
	movdqa [quarter_round_tmp.b], xmm1
	movdqa [quarter_round_tmp.c], xmm2
	movdqa [quarter_round_tmp.d], xmm3
	mov eax, [quarter_round_tmp.a]
	mov [rdi], eax
	mov eax, [quarter_round_tmp.b]
	mov [rsi], eax
	mov eax, [quarter_round_tmp.c]
	mov [rdx], eax
	mov eax, [quarter_round_tmp.d]
	mov [rcx], eax
	ret

two_rounds:
	movdqa xmm5, [chacha20_mask8]
	movdqa xmm6, [chacha20_mask16]
	push rdi
	mov rsi, rdi
	mov rdi, twenty_rounds_tmp
	mov rdx, 64
	call memcpy
	movdqa xmm0, [twenty_rounds_tmp]
	movdqa xmm1, [twenty_rounds_tmp+16]
	movdqa xmm2, [twenty_rounds_tmp+32]
	movdqa xmm3, [twenty_rounds_tmp+48]
	chacha20_two_rounds xmm0, xmm1, xmm2, xmm3, xmm4, xmm5, xmm6
	movdqa [twenty_rounds_tmp],    xmm0
	movdqa [twenty_rounds_tmp+16], xmm1
	movdqa [twenty_rounds_tmp+32], xmm2
	movdqa [twenty_rounds_tmp+48], xmm3
	pop rdi
	mov rsi, twenty_rounds_tmp
	mov rdx, 64
	call memcpy
	ret

twenty_rounds:
	movdqa xmm5, [chacha20_mask8]
	movdqa xmm6, [chacha20_mask16]
	push rdi
	mov rsi, rdi
	mov rdi, twenty_rounds_tmp
	mov rdx, 16*4
	call memcpy
	chacha20_twenty_rounds xmm0, xmm1, xmm2, xmm3, xmm4, xmm5, xmm6, twenty_rounds_tmp
	pop rdi
	mov rsi, twenty_rounds_tmp
	mov rdx, 16*4
	call memcpy
	ret

[section .bss]
align 16
rotate_left_7_tmp: resb 16
quarter_round_tmp:
.a: resb 16
.b: resb 16
.c: resb 16
.d: resb 16
twenty_rounds_tmp: resb (16*4)

; vi: cc=80 ft=nasm
