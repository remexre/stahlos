bits 64

extern forth_last_builtin
extern forth_docolon.cfa
extern forth_exit.cfa

global forth_last_pseudobuiltin

[section .forth_builtins]

; This is a smudged, no-name, no-op word, as a marker and safety guard.
forth_last_pseudobuiltin:
	dd forth_last_builtin
	db 0x02, 0
.cfa:
	NEXT

; vi: cc=80 ft=nasm
