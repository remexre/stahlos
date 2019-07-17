bits 32

extern gdtr
extern ipb.mb2
extern p3
extern p4
extern start64

global start32
global start32.end

[section .text32]

; The entry point to the kernel.
start32:
	; Store the address of the Multiboot2 structure.
	mov [ipb.mb2], ebx

	; Enable PAE.
	mov eax, cr4
	or eax, (1 << 5)
	mov cr4, eax

	; Initialize the p4 page table to point to the p3 table.
	mov eax, p3
	or al, 0x03
	mov [p4], eax

	; Initialize the p3 page table to identity map 512GiB.
	mov eax, 0x83
	mov ecx, 512
	xor edx, edx
	xor esi, esi
	mov edi, p3
.p3_init_loop:
	mov [edi], eax
	mov [edi+4], esi
	add eax, (1 << 30)
	setc dl
	add esi, edx
	add edi, 8
	loop .p3_init_loop

	; Enable SSE
	mov eax, cr0
	and ax, 0xfffb
	or ax, 0x2
	mov cr0, eax
	mov eax, cr4
	or ax, (1<<10) | (1<<9)
	mov cr4, eax

	; Set the page table.
	mov eax, p4
	mov cr3, eax

	; Enable long mode.
	mov ecx, 0xc0000080
	rdmsr
	or eax, (1 << 8)
	wrmsr

	; Enable paging.
	mov eax, cr0
	or eax, (1 << 31)
	mov cr0, eax

	; Load the GDT, then jump to start64.
	lgdt [gdtr]
	jmp 0x8:start64
.end:

; vi: cc=80 ft=nasm
