%macro dbg 1
	; The register movement shenanigans are because rsi is an ABI-preserved
	; register, and rax is not.
	mov rax, rsi
	mov rsi, %%str
%strlen len %1
	mov rcx, len
	rep outsb
	jmp %%after
%%str: db %1
%%after:
	mov rsi, rax
%endmacro

; vi: cc=80 ft=nasm
