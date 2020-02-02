OUTPUT_FORMAT(elf64-littleaarch64)
ENTRY(_start)

MEMORY {
	kernel : ORIGIN = 0x00280000, LENGTH = 0x00080000
}

SECTIONS {
	.text : { *(.text) } > kernel
	.rodata : { *(.rodata) } > kernel
}
