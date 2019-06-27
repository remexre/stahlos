bits 64

extern aes_encrypt
extern aes_mode_encrypt

global aes_do_encrypt

[section .text]

aes_do_encrypt:
	movdqa xmm0, [rdi]
	call aes_mode_encrypt
	movdqa xmm11, [rsi]
	call aes_encrypt
	movdqa [rdx], xmm11
	ret
