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

defcolon drop2, "2DROP"
	word drop
	word drop
endcolon

defcolon dup2, "2DUP"
	wordl over
	wordl over
endcolon

defcolon evaluate, "EVALUATE"
	; Save the old source.
	wordl source
	word to_in
	word fetch
	; ( ... input-addr input-len source-addr source-len source-off )
	word to_r
	word to_r
	word to_r
	; ( ... input-addr input-len )

	; Set the new source.
	word source_length
	word store
	word source_buffer
	word store
	; ( ... )

	; Reset the input position to 0.
	lit 0
	word to_in
	word store

	; Interpret the source.
	;wordl interpret ; TODO
	lit forth_isspace.cfa
	wordl next_source_pos
	wordl dot_s

	wordl debug

	; Restore the old source.
	; ( ... )
	word from_r
	word source_buffer
	word store
	word from_r
	word source_length
	word store
	word from_r
	word to_in
	word store
endcolon

defcolon interpret, "INTERPRET"
	; TODO
endcolon

defcolon isspace, "IS-SPACE?"
	lit ' '
	word incr
	wordl dot_s
	word u_less
endcolon

defcolon next_source_pos, "NEXT-SOURCE-POS"
	; ( xt -- pos )
	wordl source
	word rot
	wordl string_find_pred
endcolon

defcolon not_equal, "<>"
	word equal
	word invert
endcolon

defcolon over, "OVER"
	word to_r
	word dup
	word from_r
	word swap
endcolon

defcolon source, "SOURCE"
	word source_buffer
	word fetch
	word source_length
	word fetch
endcolon

defcolon string_find_pred, "STRING-FIND-PRED"
	; ( addr len xt -- pos ), pos == len if none found
	word to_r
	word over
	word to_r
	word add
	word from_r

.loop:
	word dup2
	wordl not_equal
	word if_impl
	dq .end

	word dup
	word fetch_char

	word r_fetch
	word execute
	word invert
	word if_impl
	dq .end

	word incr

	word jump
	dq .loop

.end:
	word swap
	word from_r
	wordl drop2
	wordl debug
endcolon

;;; Testing Words
;;; These should be replaced (probably to send messages to some other process).

defcolon debug, "DEBUG"
	wordl dot_s
	word bochs_bp
endcolon

defcolon dot, "."
	wordl dot_nosp
	wordl space
endcolon

defcolon dot_nosp, "N."
	word n_to_str
	wordl type
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
	dq .end

	word dup
	word pick
	wordl space
	wordl dot_nosp
	lit 1
	word sub
	word jump
	dq .loop
.end:
	word drop
	lit `\n`
	wordl emit
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
.loop:
	word dup
	word if_impl
	dq .end

	word over
	word fetch_char
	wordl emit

	lit 1
	word adjust_string
	word jump
	dq .loop
.end:
	word drop
	word drop
endcolon

; This is a smudged, no-name, no-op word, as a marker and safety guard.
defcode last_pseudobuiltin, "", 0, 0x02
endcode

; vi: cc=80 ft=nasm
