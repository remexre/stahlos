bits 64

extern int_init
extern int_register_all
extern pic8259_init
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

	; Create a temporary stack -- since we won't need any more 32-bit code, we
	; can reuse it as the stack.
	mov rsp, start32.end

	cli
	call pic8259_init ; Remap the PIC.
	call int_register_all ; Register all the default interrupt handlers.
	call int_init ; Set the IDT.
	sti ; Enable interrupts.

.loop:
	hlt
	jmp .loop

; vi: cc=80 ft=nasm
