bits 64

%include "src/amd64/kernel/macros.inc"
%define last_defined_word forth_last_builtin

extern forth_docolon.impl
extern forth_exit.cfa
extern forth_last_builtin

global forth_last_pseudobuiltin

[section .forth_builtins]

defcolon comma, ","
	word here
	lit 8
	word allot
	word store
endcolon

defcolon evaluate, "EVALUATE"
	; TODO evaluate
	word bochs_bp
endcolon

;;; Testing Words
;;; These should be replaced (probably to send messages to some other process).

defcolon emit, "EMIT"
	lit 0xe9
	word outb
endcolon

; This is a smudged, no-name, no-op word, as a marker and safety guard.
defcode last_pseudobuiltin, "", 0, 0x02
endcode

; vi: cc=80 ft=nasm
