bits 64

extern forth_last_pseudobuiltin
extern ifa
extern undefined_word

global gdtr
global idt
global idt.end
global idtr
global ipb ; The Important Pointer Block.
global ipb.free_list
global ipb.here
global ipb.mb2
global ipb.quantum
global ipb.quantum_max
global p3 ; aka PDPT
global p4 ; aka PML4

[section .ipb]

ipb:
	db "IPB HERE"
.mb2:
	dq 0 ; Gets filled in with address of multiboot2 information structure.
.free_list:
	dq 0 ; Head of free list for allocators.
.here:
	dq ifa ; The address returned by HERE and increased by ALLOT.
	dq p4 ; The current level 4 page table.
	dq undefined_word ; The hook to call when an undefined word is encountered.
.quantum:
	dq 0 ; The number of ticks remaining in the quantum.
.quantum_max:
	dq 10 ; The number of total ticks per quantum.
.run_queue:
	dq 0 ; Address of the run queue
.int_queue:
	dq 0 ; Address of the interrupt process queue
	dq ipb

[section .bss]

align 4096
p3: resq 512
p4: resq 512

[section .gdt]

%macro gdt_entry 3 ; base, limit, type (access byte)
	db ( %2        & 0xff)
	db ((%2 >>  8) & 0xff)
	db ( %1        & 0xff)
	db ((%1 >>  8) & 0xff)
	db ((%1 >> 16) & 0xff)
	db %3
	db ((%2 >> 16) & 0x0f) | 0xa0
	db ((%1 >> 24) & 0xff)
%endmacro

gdtr:
.size:   dw gdt.end - gdt - 1
.offset: dq gdt

gdt:
.null: dq 0
.code: gdt_entry 0, 0xfffff, 0x9a
.data: gdt_entry 0, 0xfffff, 0x92
.end:

[section .data]

idtr:
.size:   dw idt.end - idt - 1
.offset: dq idt

[section .bss]

idt: times 48 resb 16
.end:

; vi: cc=80 ft=nasm
