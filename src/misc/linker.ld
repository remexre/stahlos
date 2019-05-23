ENTRY(start32)
OUTPUT_FORMAT(elf64-x86-64)

SECTIONS {
	. = 1M;
	.kernel : {
		KEEP(*(.multiboot2_header))
		*(.text)
	}
	.kernel_data : {
		*(.data)
		*(.gdt)
		*(.forth_code)
	}
	.bss : { *(.bss) }

	. = 2M - 32;
	.ipb : {
		/* The "Important Pointer Block" */
		KEEP(*(.ipb))
	}
	ASSERT((. == 2M), "IPB has the wrong size")

	.dictionary : { *(.dictionary) }
	ASSERT((. == 64M), "Dictionary has the wrong size")
}
