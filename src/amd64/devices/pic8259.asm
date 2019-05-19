bits 64

global pic8259_init

[section .text]

%define pic1_cmd  0x20
%define pic1_data 0x21
%define pic2_cmd  0xa0
%define pic2_data 0xa1

%macro write 2
	mov al, %2
	mov dx, %1
	out dx, al
%endmacro

pic8259_init:
	; Initialize the first PIC.
	write pic1_cmd,  0x11 ; Initializing, expect 3 more "init words" on the
	                      ; data port.
	write pic1_data, 0x20 ; Vector Offset
	write pic1_data, 0x04 ; Master
	write pic1_data, 0x01 ; 8086 Mode
	write pic1_data, 0xff ; Disable all IRQs

	; Initialize the second PIC.
	write pic2_cmd,  0x11 ; Initializing, expect 3 more "init words" on the
	                      ; data port.
	write pic2_data, 0x28 ; Vector Offset
	write pic2_data, 0x02 ; Slave
	write pic2_data, 0x01 ; 8086 Mode
	write pic2_data, 0xff ; Disable all IRQs

	ret

; vi: cc=80 ft=nasm
