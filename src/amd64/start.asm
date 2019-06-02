bits 64

%include "src/amd64/macros.inc"
%include "src/amd64/kernel/macros.inc"

extern cold_exited
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
	mov r13, rsp
	mov rbp, 0x100080
	mov r14, rbp

	call pic8259_init ; Remap the PIC.
	call int_register_all ; Register all the default interrupt handlers.
	call int_init ; Set the IDT.
	sti ; Enable interrupts.

	; Start up the Forth system.
	dbg `\nstarting up forth...\n\n`
	sub rbp, 8
	mov qword [rbp], .after
	mov rsi, .pfa
	lodsq
	jmp rax
.after: dq cold_exited
.pfa:
begincolon
	word here
	; 64k bits for readiness
	; 64k qwords for address of user area
	lit (1 << 16)*9/8
	word allot
endcolon

[section .forth_code]

cold_code:
	incbin "src/amd64/forth/std.f"
	db 0x0a
	incbin "src/amd64/forth/startup.f"
.len equ $-cold_code

; vi: cc=80 ft=nasm
