bits 64

extern idt
extern idt.end
extern idtr
extern pic8259_unmask

global int_init
global int_register
global int_register_all

[section .text]

int_init:
	lidt [idtr]
	ret

; rdx - Address of interrupt handler.
; rcx - Interrupt number.
int_register:
	; Save the flags, then disable interrupts.
	pushfq
	cli

	; rdi is the address of the IDT entry.
	lea rdi, [rcx*8]
	lea rdi, [idt+rcx*8+rdi]

	; Check if the IDT has enough space; if not, skip this.
	cmp rdi, idt.end
	jae .end_bp

	; Set up the IDT descriptor.
	mov [rdi], dx
	mov word [rdi+2], 0x0008
	mov word [rdi+4], 0x8e00
	shr rdx, 16
	mov [rdi+6], dx
	shr rdx, 16
	mov [rdi+8], edx
	xor edx, edx
	mov [rdi+12], edx

	; Check if this is an interrupt on the PIC.
	cmp rcx, 0x20
	jb .end
	cmp rcx, 0x30
	jae .end

.pic:
	; Unmask the interrupt in the PIC.
	sub rcx, 0x20
	call pic8259_unmask

.end:
	; Restore the flags, possibly reenabling interrupts.
	popfq
	ret
.end_bp:
	xchg bx, bx
	popfq
	ret

int_register_all:
	mov rdx, de_handler
	mov rcx, 0 ; Divide-By-Zero
	; call int_register

	mov rdx, nmi_handler
	mov rcx, 2 ; Non-Maskable Interrupt
	; call int_register

	mov rdx, bp_handler
	mov rcx, 3 ; Breakpoint
	; call int_register

	mov rdx, ud_handler
	mov rcx, 6 ; Invalid Opcode
	; call int_register

	mov rdx, df_handler
	mov rcx, 8 ; Double Fault
	; call int_register

	mov rdx, gp_handler
	mov rcx, 13 ; General Protection Fault
	; call int_register

	mov rdx, pf_handler
	mov rcx, 14 ; Page Fault
	; call int_register

	ret

[section .text]

; The Divide-By-Zero handler.
de_handler:
	xchg bx, bx
	jmp de_handler

; The Non-Maskable Interrupt handler.
nmi_handler:
	iretq

; The Breakpoint handler.
bp_handler:
	iretq

; The Invalid Opcode handler.
ud_handler:
	xchg bx, bx
	jmp ud_handler

; The Double Fault handler.
df_handler:
	xchg bx, bx
	jmp df_handler

; The General Protection Fault handler.
gp_handler:
	xchg bx, bx
	jmp gp_handler

; The Page Fault handler.
pf_handler:
	xchg bx, bx
	jmp pf_handler

; vi: cc=80 ft=nasm
