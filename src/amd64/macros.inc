%macro dbg 1
%strlen len %1
%assign i 1
%rep len
%substr ch %1 i
	mov al, ch
	out 0xe9, al
%assign i i+1
%endrep
%endmacro

; vi: cc=80 ft=nasm
