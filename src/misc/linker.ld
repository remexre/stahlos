ENTRY(start32)
OUTPUT_FORMAT(elf64-x86-64)

/* NOTE: It's important that the entire kernel fits between 1 and 3 MiB -- the
 * allocator needs to remove this region from the allocatable memory, otherwise
 * eventually we might hand out part of the kernel as allocated memory by
 * accident! */

SECTIONS {
	. = 1M;
	.kernel32 : {
		KEEP(*(.multiboot2_header))
		*(.text32)
	}

	.kernel : {
		*(.text)
		*(.rodata)
		*(.forth_builtins)
	}
	.kernel_data : {
		*(.data)
		*(.gdt)
		*(.forth_code)
	}
	.bss : { *(.bss) }

	. = 2M - 56;
	.ipb : { KEEP(*(.ipb)) } /* The "Important Pointer Block" */
	ASSERT((. == 2M), "IPB has the wrong size")

	.ifa : {
		ifa = .;
		. = . + 1;
		. = ALIGN(1M);
	}
	ASSERT((. == 3M), "Initial free area has the wrong size")
}
