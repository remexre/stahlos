bits 64

global interpret

[section .text]

; Interprets the string whose address is given by r8 and whose length is given
; by r9, then jumps to the address in rsi.
interpret:
	call lex_word
	ret

; Lexes out a word from the buffer whose address is given by r8 and whose
; length is given by r9. Returns the address of the start of the token in r8,
; and the length in rcx. Returns the number of remaining characters in the
; buffer in r9. The length will be 0 when no words remain. Trashes rax.
;
; INVARIANT: r8_initial + r9_initial = r8_final + r9_final + rcx_final
lex_word:
	; Advance r8 and decrement r9 until is_space returns false or r9 is zero.
.skip_spaces_loop:
	test r9, r9
	jz .no_remaining_words

	mov al, [r8]
	call is_space
	test al, al
	jz .find_end_of_word

	inc r8
	dec r9
	jmp .skip_spaces_loop

.find_end_of_word:
	xor rcx, rcx

	; Increment rcx and decrement r9 until is_space returns true or r9 is zero.
.find_end_of_word_loop:
	cmp rcx, r9
	je .end

	mov al, [r8+rcx]
	call is_space
	test al, al
	jnz .end

	inc rcx
	jmp .find_end_of_word_loop

.no_remaining_words:
	xor rcx, rcx

.end:
	sub r9, rcx
	ret

; Checks if the character in al is a whitespace character. al is zero if it is
; not. Trashes rax.
;
; Supported whitespace characters are newline, tab, and space.
is_space:
	and rax, 0xff
	lea rax, [rax-9]
	cmp al, 1
	setbe ah
	cmp al, 0x17
	sete al
	or al, ah
	ret

; vi: cc=80 ft=nasm
