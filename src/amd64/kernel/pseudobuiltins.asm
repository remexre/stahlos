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

defcolon dup2, "2DUP"
	wordl over
	wordl over
endcolon

defcolon evaluate, "EVALUATE"
	; SAVE-INPUT beneath the string to evaluate.
	word to_r
	word to_r
	wordl save_input
	word from_r
	word from_r

	; Reset the input position to 0.
	lit 0
	word to_in
	word store

	;word 
	; TODO evaluate
	word bochs_bp
endcolon

defcolon nip, "NIP"
	word swap
	word drop
endcolon

defcolon over, "OVER"
	word to_r
	word dup
	word from_r
	word swap
endcolon

defcolon parse_name, "PARSE-NAME"
	wordl source
	word to_in
	word fetch
	word adjust_string
	wordl dot_s
	; lit forth_isspace.cfa
	; lit forth_isnotspace.cfa
	; TODO
endcolon

defcolon save_input, "SAVE-INPUT"
	wordl source
	word to_in
	word fetch
	lit 3
endcolon

defcolon source, "SOURCE"
	word source_buffer
	word source_length
	word fetch
	lit 2
endcolon

;;; Testing Words
;;; These should be replaced (probably to send messages to some other process).

defcolon dot, "."
	wordl dot_nosp
	wordl space
endcolon

defcolon dot_nosp, "."
	word d_to_s
	word bochs_bp ; TODO
endcolon

defcolon dot_s, ".S"
	word depth
	lit '<'
	wordl emit
	word dup
	wordl dot_nosp
	lit '>'
	wordl emit

.loop:
	; Go to end if n=0
	word dup
	word if_impl
	lit .end

	word dup
	word pick
	wordl space
	wordl dot_nosp
	lit 1
	word sub
	word jump
	lit .loop
.end:
	word drop
endcolon

defcolon bl, "BL"
	lit ' '
endcolon

defcolon emit, "EMIT"
	lit 0xe9
	word outb
endcolon

defcolon space, "SPACE"
	word bl
	wordl emit
endcolon

defcolon type, "TYPE"
	word dup
	word if_impl
	lit .end
	word dup2
	; TODO
.end:
	word drop
	word drop
endcolon

; This is a smudged, no-name, no-op word, as a marker and safety guard.
defcode last_pseudobuiltin, "", 0, 0x02
endcode

; vi: cc=80 ft=nasm
