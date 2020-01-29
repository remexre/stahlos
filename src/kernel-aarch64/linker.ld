OUTPUT_FORMAT(elf64-littleaarch64)
ENTRY(_start)

MEMORY {
    ram : ORIGIN = 0x80200000, LENGTH = 0x10000
}
 
SECTIONS {
	. = 1M;
	.text : { *(.text) } > ram
}
