bits 64

global pic8259_init
global pic8259_unmask
global pic8259_eoi1
global pic8259_eoi2

[section .text]

%define pic1_cmd  0x20
%define pic1_data 0x21
%define pic2_cmd  0xa0
%define pic2_data 0xa1

%macro write 2
	mov al, %2
	out %1, al
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

; rcx - IRQ number (not interrupt number!)
pic8259_unmask:
	; If the IRQ is out of bounds, NOP this.
	cmp rcx, 0x10
	jae .end_bp

	; Set cl to be the IRQ number within the PIC and ch to be the PIC number
	mov ch, cl
	shr ch, 3
	and cx, 0x17

	; Set ah to be the bitmask to AND against the Interrupt Mask Register.
	mov ah, 1
	shl ah, cl
	not ah

	; Set dx to be the I/O port to send to.
	mov dx, pic1_data
	test ch, ch ; If ch=0, skip ORing the bit to select PIC2.
	jz .pic1
	or dx, pic2_data
.pic1:

	; AND the Interrupt Mask Register by ah.
	in al, dx
	and al, ah
	out dx, al

	ret
.end_bp:
	xchg bx, bx
	ret

pic8259_eoi2:
	write pic2_cmd, 0x20
pic8259_eoi1:
	write pic1_cmd, 0x20
	ret

; vi: cc=80 ft=nasm
