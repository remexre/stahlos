bits 64

%include "src/amd64/macros.inc"
%include "src/amd64/kernel/macros.inc"

extern cold_exited
extern forth_last_pseudobuiltin
extern ifa
extern int_init
extern int_register_all
extern jmp_null
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

	; Enable the WP bit.
	mov rax, cr0
	or rax, 1<<16
	mov cr0, rax

	; Set the process area pointer to where we will eventually create the init
	; process, and the parameter and return stacks to within it.
	mov r15, ifa+(1 << 19)
	lea r14, [r15+512]
	lea r13, [r15+1024]
	mov rbp, r14
	mov rsp, r13
	mov rbx, "STACKTOP"

	call pic8259_init ; Remap the PIC.
	call int_register_all ; Register all the default interrupt handlers.
	call int_init ; Set the IDT.
	sti ; Enable interrupts.

	; Write a jump to NULL. This makes EXECUTEing an xt of 0 trap.
	mov byte [0], 0xe9
	mov dword [1], jmp_null-5

	; Start up the Forth system.
	sub rbp, 8
	mov qword [rbp], .after
	mov rsi, .pfa
	lodsq
	jmp rax
.after: dq cold_exited
.pfa:
begincolon
	; Reserve some space.
	;   64k qwords for process table.
	;   1k bytes for init's process space.
	lit (1 << 19) + (1 << 10) + 0x18
	word allot
	;   HERE should now be 0x280400

	; Set up the init process' link.
	;   Write the process pointer to the right spot.
	word process_pointer
	lit ifa + (1 << 19) + (1 << 10) + 0x10
	word store
	;   Write the link's address to the right spot.
	lit ifa + (1 << 19) + (1 << 10)
	lit ifa
	word store

	; Set up the init process' area.
	;   Write the PID.
	lit 0
	lit ifa + (1 << 19) + 0x00
	word store
	;   Write the source code start canary.
	lit "SRCSTART"
	lit ifa + (1 << 19) + 0x08
	word store
	;   Write the source code length canary.
	lit "SRC__LEN"
	lit ifa + (1 << 19) + 0x10
	word store
	;   Write the source code offset canary.
	lit "SRC__>IN"
	lit ifa + (1 << 19) + 0x18
	word store
	;   Write the dictionary pointer.
	lit forth_last_pseudobuiltin
	word fetch
	lit ifa + (1 << 19) + 0x20
	word store
	;   Write the flags.
	lit 0
	lit ifa + (1 << 19) + 0x28
	word store

	; Switch to hex, for convenience.
	word base_hex

	; Start running the code!
	lit cold_code
	lit cold_code.len
	word evaluate
endcolon

[section .forth_code]

cold_code:
	incbin "src/amd64/forth/std.fth"
	db 0x0a
	incbin "src/amd64/forth/init/mb2.fth"
	incbin "src/amd64/forth/init/mem.fth"
	incbin "src/amd64/forth/init/paging.fth"
	incbin "src/amd64/forth/init/spawn.fth"
	incbin "src/amd64/forth/init/main.fth"
.len equ $-cold_code

; vi: cc=80 ft=nasm
