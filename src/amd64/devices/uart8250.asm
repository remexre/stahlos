bits 64

extern int_register
extern pic8259_eoi1

global uart8250_init

[section .text]

COM1 equ 0x03f8

%macro write_COM1 2 ; offset, value
	mov al, %2
	mov dx, (COM1 + %1)
	out dx, al
%endmacro

; Initializes the serial ports.
uart8250_init:
	write_COM1 1, 0x00 ; disable interrupts
	write_COM1 3, 0x80 ; enable DLAB (to allow setting baud rate)
	write_COM1 0, 0x06 ; divisor of 6, i.e. 19200 baud
	write_COM1 1, 0x00
	write_COM1 3, 0x03 ; disable DLAB, set 8N1

	; TODO wat do
	write_COM1 2, 0xc7 ; enable fifo, clear them, with 14-byte threshold
	write_COM1 4, 0x0b ; IRQs enabled, RTS/DSR set

	write_COM1 1, 0x03 ; enable interrupts on data available and transmitter empty

	; Set up the ISR as IRQ4
	mov rcx, 4
	mov rdx, uart8250_isr
	jmp int_register ; Tail-call

; The ISR for the serial port.
uart8250_isr:

.end:
	call pic8259_eoi1
	iretq

; vi: cc=80 ft=nasm
