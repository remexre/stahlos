bits 64

global aes_decrypt
global aes_encrypt
global aes_mode_decrypt
global aes_mode_encrypt

[section .forth_builtins]

; This is in its own file so it's easier to test on a host system. See
; src/utils/aes_tests.c for the actual tests.

%macro make_round_keys_helper 4
	movdqa %1, %2
	aeskeygenassist xmm11, %1, %4
	pshufd xmm11, xmm11, 0xff
	movdqa xmm12, %1
	pslldq xmm12, 4
	pxor %1, xmm12
	pslldq xmm12, 4
	pxor %1, xmm12
	pslldq xmm12, 4
	pxor %1, xmm12
	pxor %1, xmm11
%if %3
	aesimc %1, %1
%endif
%endmacro

%macro make_round_keys 1
	make_round_keys_helper xmm1,  xmm0, %1, 0x01
	make_round_keys_helper xmm2,  xmm1, %1, 0x02
	make_round_keys_helper xmm3,  xmm2, %1, 0x04
	make_round_keys_helper xmm4,  xmm3, %1, 0x08
	make_round_keys_helper xmm5,  xmm4, %1, 0x10
	make_round_keys_helper xmm6,  xmm5, %1, 0x20
	make_round_keys_helper xmm7,  xmm6, %1, 0x40
	make_round_keys_helper xmm8,  xmm7, %1, 0x80
	make_round_keys_helper xmm9,  xmm8, %1, 0x1b
	make_round_keys_helper xmm10, xmm9,  0, 0x36
%endmacro

; in - xmm0 as key
; tmp - xmm11-12 as tmps
; out - xmm0-10 as round keys
aes_mode_encrypt:
	make_round_keys 0
	ret

; in - xmm0 as key
; tmp - xmm11-12 as tmps
; out - xmm0-10 as round keys
aes_mode_decrypt:
	make_round_keys 1
	ret

; in - xmm0-10 as round keys, xmm11 as ciphertext
; out - xmm11 as plaintext, xmm0-10 preserved
aes_decrypt:
	pxor xmm11, xmm10
	aesdec xmm11, xmm9
	aesdec xmm11, xmm8
	aesdec xmm11, xmm7
	aesdec xmm11, xmm6
	aesdec xmm11, xmm5
	aesdec xmm11, xmm4
	aesdec xmm11, xmm3
	aesdec xmm11, xmm2
	aesdec xmm11, xmm1
	aesdeclast xmm11, xmm0
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
