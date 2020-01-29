OUTPUT_FORMAT(elf64-littleaarch64)
ENTRY(_start)

SECTIONS {
	. = 1M;
	.text : {
		*(.text)
	}
}
