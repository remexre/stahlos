bits 64

%include "src/amd64/macros.inc"
%include "src/amd64/kernel/macros.inc"

extern cold_exited
extern ifa
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

	; Set the user area pointer to where we will eventually create the init
	; process, and the parameter and return stacks to within it.
	mov r15, ifa+((1 << 16)/8 + (1 << 16))
	lea r14, [r15+512]
	lea r13, [r15+1024]
	mov rbp, r14
	mov rsp, r13
	mov rbx, "STACKTOP"

	call pic8259_init ; Remap the PIC.
	call int_register_all ; Register all the default interrupt handlers.
	call int_init ; Set the IDT.
	sti ; Enable interrupts.

	; Start up the Forth system.
	sub rbp, 8
	mov qword [rbp], .after
	mov rsi, .pfa
	lodsq
	jmp rax
.after: dq cold_exited
.pfa:
begincolon
	; Reserve some space
	;   64k bits for readiness
	;   64k qwords for address of user area
	;   1k bytes for init's user space
	lit (1 << 16)/8 + (1 << 16) + (1<<10)
	word allot
	;   HERE should now be 0x212000

	; Set up the init process
	;   Write a bit that we're ready to run.
	lit 1
	lit ifa
	word store
	;   Write the user pointer to the right spot.
	word user_pointer
	lit ifa + (1 << 16)/8
	word store
	;   Write the source code start canary.
	lit "SRCSTART"
	lit ifa + (1 << 16)/8 + (1 << 16) + 16
	word store
	;   Write the source code length canary.
	lit "SRC__LEN"
	lit ifa + (1 << 16)/8 + (1 << 16) + 24
	word store

	; Start running the code!
	lit foo
	lit 0
	lit foo
	lit 0
	word streq
	word debug
	; lit cold_code
	; lit cold_code.len
	word evaluate
endcolon

foo: db "foo"

[section .forth_code]

cold_code:
	incbin "src/amd64/forth/std.f"
	db 0x0a
	incbin "src/amd64/forth/startup.f"
.len equ $-cold_code

; vi: cc=80 ft=nasm
