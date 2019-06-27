bits 64

global aes_encrypt
global aes_mode_encrypt

[section .forth_builtins]

; This is in its own file so it's easier to test on a host system. See
; src/utils/aes_tests.c for the actual tests.

%macro make_round_keys_helper 3
	movdqa %1, %2
	aeskeygenassist xmm11, %1, %3
	pshufd xmm11, xmm11, 0xff
	movdqa xmm12, %1
	pslldq xmm12, 4
	pxor %1, xmm12
	pslldq xmm12, 4
	pxor %1, xmm12
	pslldq xmm12, 4
	pxor %1, xmm12
	pxor %1, xmm11
%endmacro

; in - xmm0 as key
; tmp - xmm11-12 as tmps
; out - xmm0-10 as round keys
aes_mode_encrypt:
	make_round_keys_helper xmm1,  xmm0, 0x01
	make_round_keys_helper xmm2,  xmm1, 0x02
	make_round_keys_helper xmm3,  xmm2, 0x04
	make_round_keys_helper xmm4,  xmm3, 0x08
	make_round_keys_helper xmm5,  xmm4, 0x10
	make_round_keys_helper xmm6,  xmm5, 0x20
	make_round_keys_helper xmm7,  xmm6, 0x40
	make_round_keys_helper xmm8,  xmm7, 0x80
	make_round_keys_helper xmm9,  xmm8, 0x1b
	make_round_keys_helper xmm10, xmm9, 0x36
	ret

; in - xmm0-10 as round keys, xmm11 as plaintext
; out - xmm11 as ciphertext, xmm0-10 preserved
aes_encrypt:
	pxor xmm11, xmm0
	aesenc xmm11, xmm1
	aesenc xmm11, xmm2
	aesenc xmm11, xmm3
	aesenc xmm11, xmm4
	aesenc xmm11, xmm5
	aesenc xmm11, xmm6
	aesenc xmm11, xmm7
	aesenc xmm11, xmm8
	aesenc xmm11, xmm9
	aesenclast xmm11, xmm10
	ret

; vi: cc=80 ft=nasm
