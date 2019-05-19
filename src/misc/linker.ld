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
	}
	.bss : { *(.bss) }

	. = 0x123456;
	.ipb : {
		/* The "Important Pointer Block" */
		KEEP(*(.ipb))
	}
}
