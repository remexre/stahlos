OUTPUT_FORMAT(elf64-littleaarch64)
ENTRY(_start)

MEMORY {
    ram : ORIGIN = 0x00200000, LENGTH = 0xf7e00000
}
 
SECTIONS {
    . = 0x00280000;
	.text : { *(.text) } > ram
}
