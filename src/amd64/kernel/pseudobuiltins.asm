bits 64

%include "src/amd64/kernel/macros.inc"
%define last_defined_word forth_last_builtin

extern undefined_word
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

defcolon comma_char, "C,"
	word here
	lit 1
	word allot
	word store_char
endcolon

defcolon comment, "\"
	wordl source_rest
	lit forth_is_nl.cfa
	wordl string_find_pred
	word to_in
	word add_store
endcolon

defcolon compile_comma, "COMPILE,"
	word comma
endcolon

defcolon count, "COUNT"
	word dup
	word incr
	word swap
	word fetch_char
endcolon

defcolon create, "CREATE"
	wordl parse_name

	lit .str
	lit 7
	wordl type
	wordl dup2
	wordl typeln

	wordl debug
endcolon
.str: db "create "

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
	word to_r
	word to_r
	word to_r

	; Set the new source.
	word source_length
	word store
	word source_buffer
	word store

	; Reset the input position to 0.
	lit 0
	word to_in
	word store

	; Interpret the source.
	wordl interpret

	; Restore the old source.
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

defcolon find, "FIND"
	wordl find_header
	word dup
	word if_impl
	dq .end
	wordl header_to_cfa
.end:
endcolon

defcolon find_header, "FIND-HEADER"
	word ipb
	lit 24
	word add
	word to_r

.loop:
	word from_r
	word fetch
	word dup
	word to_r

	word if_impl
	dq .end

	wordl dup2

	word r_fetch
	wordl header_to_name
	wordl count

	word streq
	word if_impl
	dq .loop

.end:
	word drop
	word drop
	word from_r
endcolon

defcolon header_to_cfa, "HEADER>CFA"
	wordl header_to_name
	wordl count
	wordl add
endcolon

defcolon header_to_name, "HEADER>NAME"
	lit 9
	word add
endcolon

defcolon interpret, "INTERPRET"
.loop:
	wordl parse_name
	word dup

	word if_impl
	dq .end

	; DEBUG
	wordl dup2
	wordl typeln

	wordl dup2
	wordl find
	word dup_nonzero

	word if_impl
	dq .not_found

	wordl rev_rot
	wordl drop2

	word dup
	wordl is_immediate
	word get_state
	word invert
	word or

	word if_impl
	dq .compile

	word execute
	word jump
	dq .loop

.compile:
	wordl compile_comma
	word jump
	dq .loop

.not_found:
	dq undefined_word

.end:
	; TODO
	lit .end_str
	lit .end_str_len
	wordl typeln
	wordl debug
endcolon
.end_str: db "end of interpret"
.end_str_len equ $ - .end_str

defcolon is_immediate, "IMMEDIATE?"
	lit 8
	word add
	word fetch_char
	lit 0
	word test_flag
endcolon

defcolon is_nl, "IS-NL?"
	lit `\n`
	word equal
endcolon

defcolon is_space, "IS-SPACE?"
	lit ' '
	word incr
	word u_less
endcolon

defcolon isnt_space, "ISNT-SPACE?"
	lit ' '
	word u_greater
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

defcolon parse_name, "PARSE-NAME"
	wordl source_skip_spaces
	wordl source_rest
	wordl over
	wordl swap
	lit forth_is_space.cfa
	wordl string_find_pred
	word dup
	word to_in
	word add_store
endcolon

defcolon rev_rot, "-ROT"
	word rot
	word rot
endcolon

defcolon source, "SOURCE"
	word source_buffer
	word fetch
	word source_length
	word fetch
endcolon

defcolon source_rest, "SOURCE-REST"
	wordl source
	word to_in
	word fetch
	word adjust_string
endcolon

defcolon source_skip_spaces, "SOURCE-SKIP-SPACES"
	wordl source_rest
	lit forth_isnt_space.cfa
	wordl string_find_pred
	word to_in
	word add_store
endcolon

defcolon string_find_pred, "STRING-FIND-PRED"
	; ( addr len xt -- pos ), pos == len if none found
	word to_r
	word to_r
	word dup
	word from_r
	word over
	word to_r
	word add
	word from_r

.loop:
	wordl dup2
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
	word drop2
	word swap
	word sub
endcolon

;;; Testing Words
;;; These should be replaced (probably to send messages to some other process,
;;; and probably using DEFER/IS).

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

defcolon cr, "CR"
	lit `\n`
	wordl emit
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

defcolon typeln, "TYPELN"
	wordl type
	wordl cr
endcolon

padding: db "WT" ; F: if I remove this, no code runs???

; This is a smudged, no-name, no-op word, as a marker and safety guard.
defcode last_pseudobuiltin, "", 0, 0x02
endcode

; vi: cc=80 ft=nasm
