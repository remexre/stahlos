OUTPUT_FORMAT(elf64-littleaarch64)
ENTRY(_start)

SECTIONS {
    . = 0x00280000;
	.text : { *(.text) }
}
