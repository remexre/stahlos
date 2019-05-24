bits 64

extern alloc_init
extern forth_cold
extern int_init
extern int_register_all
extern interpret
extern pic8259_init
extern uart8250_init
extern start32.end

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

	call alloc_init ; Set up the allocator.
	xchg bx, bx
	; call uart8250_init ; Start the serial driver.
	call forth_cold ; Sets up the Forth system initially.

.loop:
	hlt
	jmp .loop

; vi: cc=80 ft=nasm
