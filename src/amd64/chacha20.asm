bits 64

%macro prold 3 ; dst, src, tmp
	mov %3, %1
	pslld %1, %2
	psrld %3, %2
	por %1, %3
%endmacro

%macro qr 7 ; a, b, c, d, tmp, mask8, mask16
	paddd %1, %2
	pxor %4, %1
	pshufb %4, %7

	paddd %3, %4
	pxor %2, %3
	prold %2, 12, %5

	paddd %1, %2
	pxor %4, %1
	pshufb %4, %6

	paddd %3, %4
	pxor %2, %3
	prold %2, 7, %5
%endmacro

[section .text]

[section .rodata]

align 16
mask8:  dd 0x02010003, 0x07060504, 0x0a09080b, 0x0e0d0c0f
mask16: dd 0x01000302, 0x05040706, 0x09080b0a, 0x0d0c0f0e

; vi: cc=80 ft=nasm
