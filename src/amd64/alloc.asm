bits 64

%include "src/amd64/macros.inc"

extern ipb.free_list
extern ipb.mb2
extern panic

global alloc
global alloc_init
global dealloc

[section .text]

; Initializes the allocator from the memory maps in the multiboot2 information
; structure. Preserves all registers.
alloc_init:
	sub rsp, 8*7
	mov [rsp+48], rax
	mov [rsp+42], rcx
	mov [rsp+32], rdx
	mov [rsp+24], rdi
	mov [rsp+16], r8
	mov [rsp+8], r9
	mov [rsp], r10

	mov rax, [ipb.mb2]
	add rax, 8

.find_mmap_loop:
	mov rdx, [rax]
	cmp edx, 6
	je .create_free_list

	test edx, edx
	jz panic

	shr rdx, 32
	lea rax, [rax+rdx+7]
	mov edx, 7
	not rdx
	and rax, rdx

	jmp .find_mmap_loop

.create_free_list:
	mov ecx, [rax+8] ; ecx = size of one entry
	shr rdx, 32
	sub edx, 16 ; edx = size of all remaining entries
	add rax, 16 ; rax = address of start of current entry

.create_free_list_loop:
	; Skip the entry if the type isn't "available RAM."
	mov r8d, [rax+16]
	cmp r8d, 1
	jne .create_free_list_continue

	mov r8, [rax] ; r8 = start address
	mov r9, [rax+8] ; r9 = length

	; Check if this overlaps the kernel.
	cmp r8, 0x200000
	ja .no_overlap
	lea rdi, [r8+r9]
	cmp rdi, 0x100000
	jbe .no_overlap

	mov r10, r9
	mov r9, r8
	sub r9, 0x100000
	jle .overlap_2
	call .add_to_free_list

.overlap_2:
	lea r9, [r8+r10]
	mov r8, 0x200000
	sub r9, r8
	jle .create_free_list_continue
	; Intentional fall-through

.no_overlap:

	; TODO: Check if we have >512GiB RAM? This would go out of the bounds of
	; the page table that's allocated back in start32.

	; Add the area to the free list.
	call .add_to_free_list

.create_free_list_continue:
	add rax, rcx
	sub edx, ecx
	jnz .create_free_list_loop

	mov [ipb.free_list], rdi

	mov rax, [rsp+48]
	mov rcx, [rsp+40]
	mov rdx, [rsp+32]
	mov rdi, [rsp+24]
	mov r8, [rsp+16]
	mov r9, [rsp+8]
	mov r10, [rsp]
	add rsp, 8*7
	ret

; Takes the address in r8, length in r9. Trashes rdi.
.add_to_free_list:
	; If we're literally address 0, skip the first qword there. And write a
	; canary, while we're at it.
	test r8, r8
	jnz .add_to_free_list_2

	mov rdi, "NULLNULL"
	mov qword [r8], rdi
	add r8, 8
	sub r9, 8

.add_to_free_list_2:
	; If we have fewer than 32 bytes in the allocation, we can't store a free
	; node!
	sub r9, 32
	ja .add_to_free_list_3
	ret

.add_to_free_list_3:
	dbg `.add_to_free_list_3\n`
	mov [r8], r9
	mov rdi, [ipb.free_list]
	mov [r8+8], rdi
	mov [r8+r9+16], r9
	mov edi, "FREE"
	mov qword [r8+r9+24], rdi
	mov [ipb.free_list], r8
	ret

; Allocates memory from the kernel allocator. Takes the minimum length in rax.
; Returns a pointer to the block in rax, or 0 if no such allocation was
; possible. Preserves all registers.
alloc:
	; TODO: Save registers.
	; TODO: Restore registers.
	ret

; Returns a block of memory pointed at by rax to the kernel allocator.
; Preserves all registers.
dealloc:
	; TODO: Save registers.
	; TODO: Restore registers.
	ret

; vi: cc=80 ft=nasm
