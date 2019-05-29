bits 64

extern forth_cold
extern int_init
extern int_register_all
extern pic8259_init

global start64

[section .text]

; The entry point to the kernel.
start64:
	; Set the segment registers (other than CS) to the data segment; CS must
	; already have been set for us to get to this point.
	mov ax, 0x10
	mov ds, ax
	mov es, ax
	mov fs, ax
	mov gs, ax
	mov ss, ax

	; Early on, Forth code will still need a parameter and return stack. We can
	; reuse the memory given for 32-bit code as these stacks.
	mov rsp, 0x100100
	mov rbp, 0x100080

	call pic8259_init ; Remap the PIC.
	call int_register_all ; Register all the default interrupt handlers.
	call int_init ; Set the IDT.
	sti ; Enable interrupts.

	call forth_cold ; Sets up the Forth system initially.

.loop:
	hlt
	jmp .loop

; vi: cc=80 ft=nasm
